package org.aqa

import java.io.File
import scala.xml.XML
import scala.collection.mutable.ArrayBuffer
import org.aqa.web.WebUtil
import edu.umro.ScalaUtil.FileUtil
import org.aqa.db.Institution
import scala.collection.Seq
import com.pixelmed.dicom.AttributeList
import edu.umro.ScalaUtil.DicomUtil
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.Attribute
import org.aqa.db.DicomAnonymous
import edu.umro.ScalaUtil.Trace

/** Utilities to support database anonymization. */

object AnonymizeUtil extends Logging {

  /** Used to generate alias ids for institutions. */
  val institutionAliasPrefixId = "INST"
  val institutionAliasUrlPrefixId = institutionAliasPrefixId + "_URL"
  val institutionAliasDescriptionPrefixId = institutionAliasPrefixId + "_DESCRIPTION"

  /** Used to generate alias ids for machines. */
  val machineAliasPrefixId = "MACH"
  val machineAliasNotesPrefixId = machineAliasPrefixId + "_NOTES"

  /** Used to generate alias ids for users. */
  val userAliasPrefixId = "USER"
  val userAliasFullNamePrefixId = userAliasPrefixId + "_FULLNAME"
  val userAliasEmailPrefixId = userAliasPrefixId + "_EMAIL"

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

    val aliasName = {
      if (institution.name.startsWith(institutionAliasPrefixId)) institution.name
      else aliasify(institutionAliasPrefixId, institution.institutionPK.get)
    }

    private lazy val cipher = Crypto.getCipher(key)

    lazy val name_real = if (institution.name_real.isDefined) Crypto.decryptWithNonce(institution.name_real.get, cipher) else institution.name

    /** To avoid keeping credentials in memory longer than necessary, give them an expiration time when they are removed from memory. */
    private val timeout = System.currentTimeMillis + cacheTimeout
    def isValid = {
      timeout >= System.currentTimeMillis
    }

    def encrypt(clearText: String): String = Crypto.encryptWithNonce(clearText, cipher)

    def decrypt(clearText: String): String = Crypto.decryptWithNonce(clearText, cipher)

    /**
     * Save as XML file.
     */
    def persist = {
      val file = credentialFile(aliasName)
      try {
        if (!securityDir.isDirectory) securityDir.mkdirs
        val xml = {
          <InstitutionCredentials>
            <name>{ aliasName }</name>
            <name_real>{ name_real }</name_real>
            <creationTime>{ Util.standardDateFormat.format(System.currentTimeMillis) }</creationTime>
            <key>{ key }</key>
          </InstitutionCredentials>
        }
        val xmlText = WebUtil.xmlToText(xml)
        file.delete
        Util.writeBinaryFile(file, xmlText.getBytes)
        logger.info("Wrote credential file " + file.getAbsolutePath)
      } catch {
        case t: Throwable => {
          logger.warn("Unable to write credential file " + file.getAbsolutePath + " : " + t + "\n" + fmtEx(t))
        }
      }
    }
  }

  private def readSecurityCredentials = {

    def readCredentialFile(file: File): Option[InstitutionCredentials] = {
      try {
        val doc = XML.loadFile(file)
        val name = (doc \ "name").head.text.toString
        val key = (doc \ "key").head.text.toString

        Institution.getInstitutionByName(name) match {
          case Some(institution) => Some(new InstitutionCredentials(institution, key))
          case _ => {
            // TODO this code will be obsolete when anonymization is complete, but until then
            // assume that the primary key was used to construct the institution's alias.

            val instPK = name.replaceAll(".*_", "").toLong
            val institution = Institution.get(instPK).get
            Some(new InstitutionCredentials(institution, key))
          }
        }
      } catch {
        case t: Throwable => None
      }
    }
    val list = securityDir.listFiles.filter(f => isCredentialFile(f)).map(f => readCredentialFile(f)).flatten.toList
    list
  }

  /**
   * A cache of institution credentials.  It is periodically purged to avoid keeping credentials in memory longer than necessary.  The
   * point (aka: 'necessary') is to avoid having to re-read credentials on every institution reference.
   */
  private val institutionCache = ArrayBuffer[InstitutionCredentials]()

  private def addToCache(institutionCredentials: InstitutionCredentials) = institutionCache.synchronized(institutionCache.append(institutionCredentials))

  /**
   * Get the credentials of the given institution.  If they have not yet been created, then create them and
   * save them to storage.  Also do the intelligent thing with caching.
   */
  private def getInstitutionCredentials(institutionPK: Long): InstitutionCredentials = {

    institutionCache.find(ic => ic.institution.institutionPK.get == institutionPK) match {
      case Some(institutionCredentials) => institutionCredentials // if it's in the cache, then we're done
      case _ => {
        val institution = Institution.get(institutionPK).get
        // Get it from the stored list
        readSecurityCredentials.find(ic => ic.institution.institutionPK.get == institutionPK) match {
          case Some(institutionCredentials) => {
            addToCache(institutionCredentials)
            institutionCredentials
          }
          // if it's not in the stored list, then create it, write it to the stored list, add it to the cache, and return it
          case _ => {
            val institutionCredentials = new InstitutionCredentials(institution, Crypto.makeRandomCipherKey)
            institutionCredentials.persist
            addToCache(institutionCredentials)
            institutionCredentials
          }
        }
      }
    }
  }

  def scheduleCacheExpiration: Unit = {
    /**
     * Constructing this class starts a thread that will run in the future and remove expired credentials.
     */
    class ExpireCache extends Runnable {
      def run = {
        // add a little extra time to ensure that the operation that cached the credential is done
        Thread.sleep(cacheTimeout + 1000)
        if (institutionCache.nonEmpty) {
          institutionCache.synchronized({
            val before = institutionCache.size
            val valid = institutionCache.filter(ic => ic.isValid)
            institutionCache.clear
            institutionCache.insertAll(0, valid)
            //logger.info("Cleared anonymizing security cache from size " + before + " to size " + institutionCache.size)
          })
        }
      }
      (new Thread(this)).start
    }
    new ExpireCache
  }

  /**
   * Get the key of the given institution.
   */
  def getInstitutionKey(institutionPK: Long): String = getInstitutionCredentials(institutionPK).key

  def encryptWithNonce(institutionPK: Long, text: String): String = {
    val institutionCredentials = getInstitutionCredentials(institutionPK).encrypt(text)
    scheduleCacheExpiration
    institutionCredentials
  }

  def decryptWithNonce(institutionPK: Long, text: String): String = {
    val institutionCredentials = getInstitutionCredentials(institutionPK).decrypt(text)
    scheduleCacheExpiration
    institutionCredentials
  }

  /** Minimum number of characters to be occupied by alias number. */
  private val aliasMinNumLength = 4

  /** Character to be used to prefix alias numbers that are less than the required number of digits. */
  private val aliasNumPrefixChar = "_"

  /**
   * Construct an alias from the given prefix and number.
   */
  def aliasify(aliasPrefix: String, number: Long): String = {

    /** Convenience prefix used for constructing alias numbers. */
    val aliasMinNumPrefix = Seq.fill(aliasMinNumLength)(aliasNumPrefixChar).mkString
    val numText = {
      val t = number.toString
      if (t.size < aliasMinNumLength) (aliasMinNumPrefix + t).takeRight(aliasMinNumLength) else t
    }

    aliasPrefix + numText
  }

  private def getListOfAttributesNeedingAnonymization(al: AttributeList): Seq[Attribute] = {
    val tagSet = Config.ToBeAnonymizedList.keys.toSet
    DicomUtil.findAll(al, tagSet)
  }

  /**
   * Given the list of attributes that need to be anonymized, return a list of DicomAnonymous entries that
   * address them.  If the entries do not already exist in the database, then create and insert them.
   */
  private def makeDicomAnonymousList(institutionPK: Long, attrList: Seq[Attribute]): Seq[DicomAnonymous] = {
    val institutionKey = getInstitutionKey(institutionPK)
    val previouslyAnonymized = DicomAnonymous.getAttributes(institutionPK, attrList)

    def alreadyHasBeenAnonyimized(prev: Seq[DicomAnonymous], attr: Attribute): Boolean = {
      val attrHash = DicomAnonymous.makeAttributeHash(institutionKey, attr)
      val attrText = DicomAnonymous.formatAnonAttributeTag(attr.getTag)
      prev.find(da => da.attributeHash.equals(attrHash) && da.attributeTag.equals(attrText)).isDefined
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

  def anonymizeDicom(institutionPK: Long, source: AttributeList): AttributeList = {
    val institutionKey = getInstitutionKey(institutionPK)
    val dest = DicomUtil.clone(source) // do not modify the input

    val attrList = getListOfAttributesNeedingAnonymization(dest)
    val updatedDicomAnonymousList = makeDicomAnonymousList(institutionPK, attrList)

    def anonymizeAttribute(attr: Attribute): Unit = {
      val attrHash = DicomAnonymous.makeAttributeHash(institutionKey, attr)
      val attrText = DicomAnonymous.formatAnonAttributeTag(attr.getTag)
      val anonValue = updatedDicomAnonymousList.find(da => da.attributeHash.equals(attrHash) && da.attributeTag.equals(attrText)).get.value
      attr.removeValues
      attr.addValue(anonValue)
    }

    attrList.map(a => anonymizeAttribute(a))
    dest
  }
}
