package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import org.aqa.Config
import org.aqa.Crypto
import edu.umro.ScalaUtil.FileUtil
import java.io.File
import edu.umro.util.Utility
import com.pixelmed.dicom.Attribute
import org.aqa.AnonymizeUtil
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Crypto
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.util.UMROGUID
import edu.umro.ScalaUtil.Trace

/**
 * Support the anonymization of DICOM by providing a way to store previously anonymized values in the database.
 */
case class DicomAnonymous(
  dicomAnonymousPK: Option[Long], // primary key
  institutionPK: Long, // associated institution
  attributeTag: String, // DICOM attribute tag
  value: String, // public value for tag
  attributeHash: String, // secure hash of tag and value to allow searches on this row.
  value_real: String // actual non-anonymized value encrypted
) {

  override def toString = {
    "PK: " + { if (dicomAnonymousPK.isDefined) dicomAnonymousPK.get else "none" } +
      "    instPK: " + institutionPK +
      "    tag: " + attributeTag +
      "    value: " + value +
      "    attrHash: " + attributeHash.take(10) + "..." +
      "    value_real: " + value_real.take(10) + "..."
  }

  /* This probably should never be used due to the special nature of generating anonymized values using
   * the primary key which is generated as the entry is inserted.

  def insert = {
    val insertQuery = DicomAnonymous.query returning
      DicomAnonymous.query.map(_.dicomAnonymousPK) into
      ((dicomAnonymous, dicomAnonymousPK) => dicomAnonymous.copy(dicomAnonymousPK = Some(dicomAnonymousPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }
  */

  def insertOrUpdate = Db.run(DicomAnonymous.query.insertOrUpdate(this))

  /**
   * Return a copy of this with the <code>value_real</code> set to the original value.
   */
  def unencrypt = {
    val vReal = AnonymizeUtil.encryptWithNonce(institutionPK, value_real)
    this.copy(value_real = vReal)
  }
}

object DicomAnonymous extends Logging {

  class DicomAnonymousTable(tag: Tag) extends Table[DicomAnonymous](tag, "dicomAnonymous") {

    def dicomAnonymousPK = column[Long]("dicomAnonymousPK", O.PrimaryKey, O.AutoInc)
    def institutionPK = column[Long]("institutionPK")
    def attributeTag = column[String]("attributeTag")
    def value = column[String]("value")
    def attributeHash = column[String]("attributeHash")
    def value_real = column[String]("value_real")

    def * = (
      dicomAnonymousPK.?,
      institutionPK,
      attributeTag,
      value,
      attributeHash,
      value_real) <> ((DicomAnonymous.apply _)tupled, DicomAnonymous.unapply _)

    def institutionFK = foreignKey("institutionPK", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[DicomAnonymousTable]

  def get(dicomAnonymousPK: Long): Option[DicomAnonymous] = {
    val action = query.filter(m => m.dicomAnonymousPK === dicomAnonymousPK)
    val list = Db.run(action.result)
    list.headOption
  }

  def listDicomAnonymousFromInstitution(institutionPK: Long): Seq[DicomAnonymous] = {
    val action = query.filter(m => m.institutionPK === institutionPK)
    val seq = Db.run(action.result)
    seq
  }

  /**
   * Delete the dicomAnonymous from the database.  If that is successful, then also delete its configuration directory.
   */
  def delete(dicomAnonymousPK: Long): Int = {
    val dicomAnonymous = get(dicomAnonymousPK)
    if (dicomAnonymous.isDefined) {
      val q = query.filter(_.dicomAnonymousPK === dicomAnonymousPK)
      val action = q.delete
      val count = Db.run(action)
      count
    } else 0
  }

  def formatAnonAttributeTag(tag: AttributeTag): String = {
    val hex = DicomUtil.formatAttrTag(tag)
    val name = {
      val n = DicomUtil.dictionary.getNameFromTag(tag)
      if (n == null) "" else " " + n
    }
    hex + name
  }

  /**
   * Make a hash of the given attribute by combining:
   *
   * institutionKey + tag + value
   *
   * This makes it possible to look up anonymized values in the database without first decrypting them.
   *
   * The institution key and tag make the hash unique across institutions and tags.
   */
  def makeAttributeHash(institutionKey: String, attr: Attribute): String = {
    val text = institutionKey + formatAnonAttributeTag(attr.getTag) + attr.getSingleStringValueOrEmptyString
    Crypto.byteArrayToHex(Crypto.secureHash(text.getBytes))
  }

  /**
   * Get the anonymous value for the given attribute.  If it is auto-incrementing (Name_number) then
   * return None and leave the generation of the value to the caller.
   */
  private def getAnonymousValue(institutionPK: Long, attr: Attribute): Option[String] = {
    val tag = attr.getTag

    val toBeAnonymized = Config.ToBeAnonymizedList(tag)

    lazy val isUid = {
      val vr = DicomUtil.dictionary.getValueRepresentationFromTag(tag)
      (vr != null) && ValueRepresentation.isUniqueIdentifierVR(vr)
    }

    val anonValue: Option[String] = 0 match {
      case _ if (toBeAnonymized.Value.isDefined) => Some(toBeAnonymized.Value.get)
      case _ if isUid => Some(UMROGUID.getUID)
      case _ => None
    }

    anonValue
  }

  /**
   * Given an attribute, insert it into the database with the appropriate fields encrypted and return the result.
   */
  def insert(institutionPK: Long, attr: Attribute): DicomAnonymous = {
    val tag = attr.getTag
    val valu = attr.getSingleStringValueOrEmptyString
    val institutionKey = AnonymizeUtil.getInstitutionKey(institutionPK)

    val tba = Config.ToBeAnonymizedList(tag)
    val anonymousValue = getAnonymousValue(institutionPK, attr)

    val da = new DicomAnonymous(
      None,
      institutionPK,
      formatAnonAttributeTag(tag),
      if (anonymousValue.isDefined) anonymousValue.get else "dummyValue",
      makeAttributeHash(institutionKey, attr),
      AnonymizeUtil.encryptWithNonce(institutionPK, valu))

    val insertQuery = {
      DicomAnonymous.query returning
        DicomAnonymous.query.map(_.dicomAnonymousPK) into
        ((dicomAnonymous, dicomAnonymousPK) => dicomAnonymous.copy(dicomAnonymousPK = Some(dicomAnonymousPK)))
    }

    val action = insertQuery += da
    val result = Db.run(action)

    if (anonymousValue.isEmpty) {
      val revised = result.copy(value = tba.Name + "_" + result.dicomAnonymousPK.get.toString)

      //      Db.run(query.filter(_.dicomAnonymousPK === result.dicomAnonymousPK.get).
      //        map(da => (da.anonymousValue).update(revised.anonymousValue))

      Db.run(query.insertOrUpdate(revised))
      revised
    } else
      result
  }

  /**
   * Get previously anonymized values for the given institution.
   */
  def getAttributes(institutionPK: Long, attrList: Seq[Attribute]): Seq[DicomAnonymous] = {
    val institutionKey = AnonymizeUtil.getInstitutionKey(institutionPK)
    val hashMap = attrList.map(attr => (makeAttributeHash(institutionKey, attr), attr)).toMap
    val hashSet = hashMap.keySet

    val action = {
      DicomAnonymous.query.filter(da => (da.institutionPK === institutionPK) && da.attributeHash.inSet(hashSet))
    }

    // show database statement
    // action.result.statements.foreach(println)

    val list = Db.run(action.result)
    list
  }

}
