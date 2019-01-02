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

/** Utilities to support database anonymization. */

object AnonymizeUtil extends Logging {

  /** Used to generate alias ids for institutions. */
  val institutionAliasPrefixId = "INST"

  /** Used to generate alias ids for machines. */
  val machineAliasPrefixId = "MACH"

  /** Used to generate alias device serial number. */
  val machineAliasPrefixSerialNumber = machineAliasPrefixId + "_SER"

  /** Used to generate alias ids for users. */
  val userAliasPrefixId = "USER"

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

    def this(name: String, key: String) = this(Institution.getInstitutionByName(name).get, key)

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
        Some(new InstitutionCredentials(name, key))
      } catch {
        case t: Throwable => None
      }
    }
    securityDir.listFiles.filter(f => isCredentialFile(f)).map(f => readCredentialFile(f)).flatten.toList
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
            logger.info("Cleared anonymizing security cache from size " + before + " to size " + institutionCache.size)
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

  def anonymizeDicom(institutionPK: Long, source: AttributeList): AttributeList = {
    val dest = DicomUtil.clone(source) // do not modify the input
    val tagSet = Config.ToBeAnonymizedList.keys.toSet
    val attrList = DicomUtil.findAll(dest, tagSet)
    val institutionKey = getInstitutionKey(institutionPK)

    //    def hashAttr(attr: Attribute): String = {
    //      val text = institutionKey + DicomUtil.formatAttrTag(attr.getTag) + attr.getSingleStringValueOrEmptyString
    //      Crypto.byteArrayToHex(Crypto.secureHash(text.getBytes))
    //    }

    val prev = DicomAnonymous.getAttributes(institutionPK, attrList).map(p => (p.attributeHash, p)).toMap
    def wasPrevAnonyimized(a: Attribute): Boolean = {
      val p = prev.get(DicomAnonymous.makeAttributeHash(institutionKey, a))
      p.isDefined && p.get.attributeTag.equals(DicomUtil.formatAttrTag(a.getTag))
    }
    val both = attrList.partition(a => wasPrevAnonyimized(a))

    val oldAttr = both._1
    val newAttr = both._2

    ??? // TODO just, TODO

    dest
  }
}
