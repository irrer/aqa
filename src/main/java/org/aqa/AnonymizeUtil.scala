/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeFactory
import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.DicomAnonymous
import org.aqa.db.Institution
import org.aqa.web.WebUtil

import java.io.File
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer
import scala.xml.Elem
import scala.xml.Node
import scala.xml.Text
import scala.xml.XML

/** Utilities to support database anonymization. */

object AnonymizeUtil extends Logging {

  /** Used to generate alias ids for institutions. */
  val institutionAliasPrefixId = "INST"
  val institutionAliasUrlPrefixId: String = institutionAliasPrefixId + "_URL"
  val institutionAliasDescriptionPrefixId: String = institutionAliasPrefixId + "_DESCRIPTION"

  /** Used to generate alias ids for machines. */
  val machineAliasPrefixId = "MACH"
  val machineAliasNotesPrefixId: String = machineAliasPrefixId + "_NOTES"
  val machineAliasTreatmentPlanningSystemId: String = machineAliasPrefixId + "_TPS_ID"

  /** Used to generate alias ids for users. */
  val userAliasPrefixId = "USER"
  val userAliasFullNamePrefixId: String = userAliasPrefixId + "_FULLNAME"
  val userAliasEmailPrefixId: String = userAliasPrefixId + "_EMAIL"

  /** Name of directory where security credentials are kept. */
  private val securityDirName = "security"

  /** Directory where security credentials are kept. */
  private lazy val securityDir = new File(Config.DataDir, securityDirName)

  private val credentialFileNamePrefix = "credential_"
  private val credentialFileNameSuffix = ".xml"

  /**
    * Given the public (alias) name of the institution, construct a file.
    */
  private def credentialFile(name: String): File = {
    val safeName = FileUtil.replaceInvalidFileNameCharacters(name, '_').replace(' ', '_')
    new File(securityDir, credentialFileNamePrefix + safeName + credentialFileNameSuffix)
  }

  private def isCredentialFile(file: File) = {
    val name = file.getName
    name.startsWith(credentialFileNamePrefix) && name.endsWith(credentialFileNameSuffix)
  }

  /** Time in ms before flushing cache. */
  private val cacheTimeout = 10 * 60 * 1000

  /**
    * For caching institution credentials.
    *
    * @param institution: Entry from database.
    *
    * @param key: Secret key as a hexadecimal string.
    */
  private case class InstitutionCredentials(institution: Institution, key: String) {

    val aliasName: String = {
      if (institution.name.startsWith(institutionAliasPrefixId)) institution.name
      else aliasify(institutionAliasPrefixId, institution.institutionPK.get)
    }

    private lazy val cipher = Crypto.getCipher(key)

    lazy val name_real: String = if (institution.name_real.isDefined) Crypto.decryptWithNonce(institution.name_real.get, cipher) else institution.name

    /** To avoid keeping credentials in memory longer than necessary, give them an expiration time when they are removed from memory. */
    private val timeout = System.currentTimeMillis + cacheTimeout
    def isValid: Boolean = {
      timeout >= System.currentTimeMillis
    }

    def encrypt(clearText: String): String = Crypto.encryptWithNonce(clearText, cipher)

    def decrypt(clearText: String): String = Crypto.decryptWithNonce(clearText, cipher)

    /**
      * Save as XML file.  The XML file is local to the server and by design not in the database.
      */
    def persist(): Unit = {
      val file = credentialFile(aliasName)
      try {
        if (!securityDir.isDirectory) securityDir.mkdirs
        val xml = {
          <InstitutionCredentials>
            <name>{aliasName}</name>
            <name_real>{name_real}</name_real>
            <creationTime>{Util.standardDateFormat.format(System.currentTimeMillis)}</creationTime>
            <key>{key}</key>
          </InstitutionCredentials>
        }
        val xmlText = WebUtil.xmlToText(xml)
        file.delete
        Util.writeBinaryFile(file, xmlText.getBytes)
        logger.info("Wrote credential file " + file.getAbsolutePath)
      } catch {
        case t: Throwable =>
          logger.warn("Unable to write credential file " + file.getAbsolutePath + " : " + t + "\n" + fmtEx(t))
      }
    }
  }

  private def readSecurityCredentials = {

    def readCredentialFile(file: File): Option[InstitutionCredentials] = {
      try {
        val doc = XML.loadFile(file)
        val name = (doc \ "name").head.text
        val key = (doc \ "key").head.text

        Institution.getInstitutionByName(name) match {
          case Some(institution) => Some(InstitutionCredentials(institution, key))
          case _                 =>
            // TODO this code will be obsolete when anonymization is complete, but until then
            // assume that the primary key was used to construct the institution's alias.

            val instPK = name.replaceAll(".*_", "").toLong
            val institution = Institution.get(instPK).get
            Some(InstitutionCredentials(institution, key))
        }
      } catch {
        case _: Throwable => None
      }
    }
    val list = securityDir.listFiles.filter(f => isCredentialFile(f)).flatMap(f => readCredentialFile(f)).toList
    list
  }

  /**
    * A cache of institution credentials.  It is periodically purged to avoid keeping credentials in memory longer than necessary.  The
    * point (aka: 'necessary') is to avoid having to re-read credentials on every institution reference.
    */
  private val institutionCache = ArrayBuffer[InstitutionCredentials]()

  private def addToCache(institutionCredentials: InstitutionCredentials): Unit = institutionCache.synchronized(institutionCache.append(institutionCredentials))

  /**
    * Get the credentials of the given institution.  If they have not yet been created, then create them and
    * save them to storage.  Also do the intelligent thing with caching.
    */
  private def getInstitutionCredentials(institutionPK: Long): InstitutionCredentials = {

    val institutionCredentialsOption = institutionCache.synchronized(institutionCache.find(ic => ic.institution.institutionPK.get == institutionPK))

    institutionCredentialsOption match {
      case Some(institutionCredentials) => institutionCredentials // if it's in the cache, then we're done
      case _ =>
        val institution = Institution.get(institutionPK).get
        // Get it from the stored list
        readSecurityCredentials.find(ic => ic.institution.institutionPK.get == institutionPK) match {
          case Some(institutionCredentials) =>
            addToCache(institutionCredentials)
            institutionCredentials
          // if it's not in the stored list, then create it, write it to the stored list, add it to the cache, and return it
          case _ =>
            val institutionCredentials = InstitutionCredentials(institution, Crypto.makeRandomCipherKey)
            institutionCredentials.persist()
            addToCache(institutionCredentials)
            institutionCredentials
        }
    }
  }

  /**
    * Start a thread that periodically looks for expired credentials in the cache and removes them.
    */
  private def initializeCacheExpiration(): Unit = {

    /**
      * Constructing this class starts a thread that will run in the future and remove expired credentials.
      */
    class ExpireCache extends Runnable {
      def run(): Unit = {
        while (true) {
          Thread.sleep(cacheTimeout)
          try {
            if (institutionCache.nonEmpty) {
              institutionCache.synchronized({
                val sizeBefore = institutionCache.size
                val valid = institutionCache.filter(ic => ic.isValid)
                institutionCache.clear
                institutionCache.insertAll(0, valid)
                logger.info("Cleared anonymizing security cache from size " + sizeBefore + " to size " + institutionCache.size)
              })
            }
          } catch {
            case t: Throwable => logger.warn("Ignoring unexpected error in cache expiration: " + fmtEx(t))
          }
        }
      }
    }

    new Thread(new ExpireCache).start()
  }

  initializeCacheExpiration()

  /**
    * Get the key of the given institution.
    */
  def getInstitutionKey(institutionPK: Long): String = getInstitutionCredentials(institutionPK).key

  def encryptWithNonce(institutionPK: Long, text: String): String = {
    val institutionCredentials = getInstitutionCredentials(institutionPK).encrypt(text)
    institutionCredentials
  }

  def decryptWithNonce(institutionPK: Long, text: String): String = {
    val institutionCredentials = getInstitutionCredentials(institutionPK).decrypt(text)
    institutionCredentials
  }

  /**
    * Construct an alias from the given prefix and number.
    */
  def aliasify(aliasPrefix: String, number: Long): String = {
    aliasPrefix + "_" + number.toString
  }

  private def getListOfAttributesThatGetAnonymized(al: AttributeList): Seq[Attribute] = {
    val tagSet = Config.ToBeAnonymizedList.keys.toSet
    DicomUtil.findAll(al, tagSet)
  }

  private val makeDicomAnonymousListSync = ""

  /**
    * Given the list of attributes that need to be anonymized, return a list of DicomAnonymous entries that
    * address them.  If the entries do not already exist in the database, then create and insert them.
    */
  def makeDicomAnonymousList(institutionPK: Long, attrList: Seq[Attribute]): Seq[DicomAnonymous] =
    makeDicomAnonymousListSync.synchronized {
      val institutionKey = getInstitutionKey(institutionPK)
      val previouslyAnonymized = DicomAnonymous.getAttributes(institutionPK, attrList)

      def alreadyHasBeenAnonyimized(prev: Seq[DicomAnonymous], attr: Attribute): Boolean = {
        val attrHash = DicomAnonymous.makeAttributeHash(institutionKey, attr)
        val attrText = DicomAnonymous.formatAnonAttributeTag(attr.getTag)
        prev.exists(da => da.attributeHash.equals(attrHash) && da.attributeTag.equals(attrText))
      }

      def addIfNeeded(list: Seq[DicomAnonymous], attr: Attribute): Seq[DicomAnonymous] = {
        if (alreadyHasBeenAnonyimized(list, attr))
          list
        else {
          val da = DicomAnonymous.insert(institutionPK, attr)
          list :+ da
        }
      }

      val updated = attrList.foldLeft(previouslyAnonymized)((list, attr) => addIfNeeded(list, attr))
      updated
    }

  private val anonymizeDicomSync = ""

  /**
    * Anonymize an attribute list.  This must be synchronized to ensure that consistency of anonymized values.
    *
    * @param institutionPK For this institution.
    * @param source DICOM to be anonymized.  This is not modified.
    * @return Anonymized version of DICOM.
    */
  def anonymizeDicom(institutionPK: Long, source: AttributeList): AttributeList =
    anonymizeDicomSync.synchronized {
      val institutionKey = getInstitutionKey(institutionPK)
      val dest = DicomUtil.clone(source) // do not modify the input

      val attrList = getListOfAttributesThatGetAnonymized(dest)
      val updatedDicomAnonymousList = makeDicomAnonymousList(institutionPK, attrList)

      def anonymizeAttribute(attr: Attribute): Unit = {
        val attrHash = DicomAnonymous.makeAttributeHash(institutionKey, attr)
        val attrText = DicomAnonymous.formatAnonAttributeTag(attr.getTag)
        val anonValue = updatedDicomAnonymousList.find(da => da.attributeHash.equals(attrHash) && da.attributeTag.equals(attrText)).get.value
        attr.removeValues()
        attr.addValue(anonValue)
      }

      attrList.foreach(a => anonymizeAttribute(a))
      dest
    }

  /**
    * De-anonymize DICOM.  Note that small errors may occur:
    *
    *    - Attributes with no values (<null>) will be restored as a single empty string.
    *
    *    - Attributes with an extra blank at the end of the value may have it removed.
    *
    *    - Attributes with no extra blank at the end of the value may have one added.
    *
    *    - Some attributes that the DICOM specification says to anonymize are not.  See
    *      configuration file for more details.
    *
    * @param institutionPK For this institution.
    * @param sourceSeq Anonymized DICOM.  This is not modified.
    * @return De-anonymized DICOM in the same order as <code>sourceSeq</code>.
    */
  def deAnonymizeDicom(institutionPK: Long, sourceSeq: Seq[AttributeList]): Seq[AttributeList] = {
    val destSeq = sourceSeq.map(DicomUtil.clone) // do not modify the input

    val attrList = destSeq.flatMap(getListOfAttributesThatGetAnonymized)

    val dicomAnonymousList = DicomAnonymous.getByAttrAndValue(institutionPK, attrList)

    def deAnonymizeAttribute(attr: Attribute): Unit = {
      val attrTag = DicomAnonymous.formatAnonAttributeTag(attr.getTag)
      val attrValue = attr.getSingleStringValueOrEmptyString().trim

      val da = dicomAnonymousList.find(da => da.attributeTag.equals(attrTag) && da.value.trim.equals(attrValue))
      if (da.isDefined) {
        val origValue = da.get.originalValue
        attr.removeValues()
        attr.addValue(origValue)
      } else {
        logger.warn("Unable to properly de-anonymize attribute " + attrTag + " " + attrValue)
      }
    }

    attrList.foreach(a => deAnonymizeAttribute(a))

    destSeq
  }

  /**
    * De-anonymize a single attribute.  If it can be deanonymized, then return a
    * copy of it as deanonymized.  If it can not be deanonymized, then return None.
    *
    * Note that an attribute can not be deanonymized if it was never anonymized.
    *
    * @param institutionPK For this institution.
    * @param attribute To be deanonymized.  Original value is not changed.
    * @return New, deanonymized attribute.
    */
  def deAnonymizeAttribute(institutionPK: Long, attribute: Attribute): Option[Attribute] = {
    val dicomAnonymousList = DicomAnonymous.getByAttrAndValue(institutionPK, Seq(attribute))
    val attrTag = DicomAnonymous.formatAnonAttributeTag(attribute.getTag)
    val attrValue = attribute.getSingleStringValueOrEmptyString().trim
    val da = dicomAnonymousList.find(da => da.attributeTag.equals(attrTag) && da.value.trim.equals(attrValue))
    if (da.isDefined) {
      val deAnon = AttributeFactory.newAttribute(attribute.getTag)
      val origValue = da.get.originalValue
      deAnon.addValue(origValue)
      Some(deAnon)
    } else
      None
  }

  /**
    * Anonymize the machine serial number (aka: DeviceSerialNumber) in the given MachineLog XML.
    *
    * @param institutionPK Owning institution.
    * @param xml Content to be anonymized.
    *
    * @return Same XML with device serial number anonymized.
    */
  def anonymizeMachineLog(institutionPK: Long, xml: Elem): Node = {
    import scala.xml.Node
    import scala.xml.transform.RewriteRule
    import scala.xml.transform.RuleTransformer

    val dsn = Util.machineLogSerialNumber(xml)

    if (dsn.isEmpty) {
      xml
    } else {

      // Use the same value as in the DICOM DeviceSerialNumber tag.  Note that if there is not already a
      // value in the database, then one will be be established.
      val anonymizedDeviceSerialNumber = {
        val at = AttributeFactory.newAttribute(TagByName.DeviceSerialNumber)
        at.addValue(dsn.get)
        val l = makeDicomAnonymousList(institutionPK, Seq(at))
        l.head.value
      }

      object t1 extends RewriteRule {
        override def transform(n: Node): Seq[Node] =
          n match {
            case Elem(prefix, "MachineSerialNumber", attributes, scope, _*) =>
              Elem(prefix, "MachineSerialNumber", attributes, scope, false, Text(anonymizedDeviceSerialNumber))
            case other => other
          }
      }

      object rt1 extends RuleTransformer(t1)

      object t2 extends RewriteRule {
        override def transform(n: Node): Seq[Node] =
          n match {
            case sn @ Elem(_, "Environment", _, _, _*) => rt1(sn)
            case other                                 => other
          }
      }

      object rt2 extends RuleTransformer(t2)

      val anonymized = rt2(xml)
      anonymized
    }
  }

  /**
    * General function for anonymizing XML.
    *
    * This handles machine log files, but can be expanded to handle other types of XLM.
    *
    * @param institutionPK  Institution PK.
    * @param xml XML to be anonymized.
    * @return XML in same form, but with fields anonymized.
    */
  def anonymizeXml(institutionPK: Long, xml: Elem): Node = {
    anonymizeMachineLog(institutionPK, xml)
  }
}
