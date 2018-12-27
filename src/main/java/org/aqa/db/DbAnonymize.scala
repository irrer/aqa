package org.aqa.db

import org.aqa.Logging
import java.io.File
import org.aqa.Config
import scala.xml.XML
import org.aqa.Crypto
import edu.umro.ScalaUtil.Trace
import scala.collection.mutable.ArrayBuffer
import org.aqa.Util
import org.aqa.web.WebUtil
import edu.umro.ScalaUtil.FileUtil

//import slick.profile.FixedSqlAction   // TODO fix

/** Anonymize Database. */

object DbAnonymize extends Logging {

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

    private lazy val cipher = Crypto.getCipher(key)

    lazy val name_real = Crypto.decrypt(institution.name_real.get, cipher)
    lazy val url_real = Crypto.decrypt(institution.url_real, cipher)
    lazy val description_real = Crypto.decrypt(institution.description_real, cipher)

    /** To avoid keeping credentials in memory longer than necessary, give them an expiration time when they are removed from memory. */
    private val timeout = System.currentTimeMillis + cacheTimeout
    def isValid = timeout < System.currentTimeMillis

    def encrypt(clearText: String): String = Crypto.encrypt(clearText, cipher)

    /**
     * Save as XML file.
     */
    def persist = {
      val file = credentialFile(institution.name)
      try {
        val xml = {
          <InstitutionCredentials>
            <name>{ institution.name }</name>
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

  /**
   * Constructing this class starts a thread that will run in the future and remove expired credentials.
   */
  private class ExpireCache extends Runnable {
    def run = {
      Thread.sleep(cacheTimeout)
      institutionCache.synchronized({
        val valid = institutionCache.filter(ic => ic.isValid)
        institutionCache.clear
        institutionCache.insertAll(0, valid)
      })
    }
    (new Thread(this)).start
  }

  def encrypt(institutionPK: Long, text: String): String = {
    getInstitutionCredentials(institutionPK).encrypt(text)
  }

  private def anonymizeMachines = {
    val list = Machine.list

    def anon(machine: Machine) = {
      val aliasId = Crypto.aliasify(Machine.aliasPrefixId, machine.machinePK.get)
      val aliasSerNum = Some(Crypto.aliasify(Machine.aliasPrefixSerialNumber, machine.machinePK.get))
      def id_real = Some(encrypt(machine.institutionPK, machine.id))
      def serNum_real = Some(encrypt(machine.institutionPK, machine.serialNumber.get))

      def anonId(m: Machine) = m.copy(id_real = id_real).copy(id = aliasId)

      def anonSerialNum(m: Machine) = m.copy(serialNumber = aliasSerNum).copy(serialNumber_real = serNum_real)

      val encryptId = machine.id_real.isEmpty

      val encryptSerNo = machine.serialNumber.isDefined && machine.serialNumber_real.isEmpty

      def update(m: Machine) = {
        def show(mch: Machine) = {
          "    id: " + mch.id.formatted("%-16s") + 
          "    mch.id_real: " + mch.id_real.toString.formatted("%-70s") + 
          "    mch.serialNumber: " + mch.serialNumber.toString.formatted("%-18s") + 
          "    mch.serialNumber_real: " + mch.serialNumber_real.toString.formatted("%-70s")
    }
        val both = show(machine) + " ==>\n" + show(m)
        logger.info("ConvertToAnonymousDatabase Need to convert machine from :\n" + both)
        if (Config.ConvertToAnonymousDatabase) {
          m.insertOrUpdate
          logger.info("ConvertToAnonymousDatabase Updating machine from :\n" + both)
        }
      }

      (encryptId, encryptSerNo) match {
        case (true, true) => update(anonId(anonSerialNum(machine)))
        case (true, false) => update(anonId(machine))
        case (false, true) => update(anonSerialNum(machine))
        case (false, false) => ; // nothing needs changing
      }

    }

    list.map(m => anon(m))
  }

  def init = {
    if (!securityDir.isDirectory) securityDir.mkdirs
    anonymizeMachines
  }
}
