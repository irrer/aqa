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

package org.aqa.db

import com.pixelmed.dicom.Attribute
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.ValueRepresentation
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.util.UMROGUID
import org.aqa.AnonymizeUtil
import org.aqa.Config
import org.aqa.Crypto
import org.aqa.Logging
import org.aqa.db.Db.driver.api._

/**
  * Support the anonymization of DICOM by providing a way to store previously anonymized values in the database.
  *
  * Note: All UID's are anonymized with the generic UID tag instead of their own (like 0008,0018:SOPInstanceUID or
  * 0008,1155:ReferencedSOPInstanceUID).  This is because the same value is used by different attributes, and the
  * lookup is really under the context of UID.
  */
case class DicomAnonymous(
    dicomAnonymousPK: Option[Long], // primary key
    institutionPK: Long, // associated institution
    attributeTag: String, // DICOM attribute tag
    value: String, // public value for tag
    attributeHash: String, // secure hash of tag and value to allow searches on this row.
    value_real: String // actual non-anonymized value encrypted
) {

  override def toString: String = {
    "PK: " + { if (dicomAnonymousPK.isDefined) dicomAnonymousPK.get else "none" } +
      "    instPK: " + institutionPK +
      "    tag: " + attributeTag +
      "    value: " + value +
      "    attrHash: " + attributeHash.take(10) + "..." +
      "    value_real: " + value_real.take(10) + "..."
  }

  def insertOrUpdate(): Int = Db.run(DicomAnonymous.query.insertOrUpdate(this))

  /**
    * Return a copy of this with the <code>value_real</code> set to the original value.
    */
  def originalValue: String = {
    val original = AnonymizeUtil.decryptWithNonce(institutionPK, value_real)
    original
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

    def * = (dicomAnonymousPK.?, institutionPK, attributeTag, value, attributeHash, value_real) <> (DicomAnonymous.apply _ tupled, DicomAnonymous.unapply)

    def institutionFK =
      foreignKey("DicomAnonymous_institutionPKConstraint", institutionPK, Institution.query)(_.institutionPK, onDelete = ForeignKeyAction.Restrict, onUpdate = ForeignKeyAction.Cascade)
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
    val workingTag =
      if (ValueRepresentation.isUniqueIdentifierVR(DicomUtil.dictionary.getValueRepresentationFromTag(tag)))
        TagFromName.UID
      else
        tag

    val hex = DicomUtil.formatAttrTag(workingTag)
    val name = {
      val n = DicomUtil.dictionary.getNameFromTag(workingTag)
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
  private def getAnonymousValue(attr: Attribute): Option[String] = {
    val tag = attr.getTag

    val toBeAnonymized = Config.ToBeAnonymizedList(tag)

    lazy val isUid = {
      val vr = DicomUtil.dictionary.getValueRepresentationFromTag(tag)
      (vr != null) && ValueRepresentation.isUniqueIdentifierVR(vr)
    }

    val anonValue: Option[String] = 0 match {
      case _ if toBeAnonymized.Value.isDefined => Some(toBeAnonymized.Value.get)
      case _ if isUid                          => Some(UMROGUID.getUID)
      case _                                   => None
    }

    anonValue
  }

  /**
    * Given an attribute, insert it into the database with the appropriate fields encrypted and return the result.
    */
  def insert(institutionPK: Long, attr: Attribute): DicomAnonymous = {
    val tag = attr.getTag
    val value = attr.getSingleStringValueOrEmptyString
    val institutionKey = AnonymizeUtil.getInstitutionKey(institutionPK)

    val tba = Config.ToBeAnonymizedList(tag)
    val anonymousValue = getAnonymousValue(attr)

    val da = new DicomAnonymous(
      None,
      institutionPK = institutionPK,
      attributeTag = formatAnonAttributeTag(tag),
      value = if (anonymousValue.isDefined) anonymousValue.get else "dummyValue",
      attributeHash = makeAttributeHash(institutionKey, attr),
      value_real = AnonymizeUtil.encryptWithNonce(institutionPK, value)
    )

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

  /**
    * Get previously anonymized values by tag for all institutions.
    */
  def getAttributesByTag(tagList: Seq[AttributeTag]): Seq[DicomAnonymous] = {
    val tagSet = tagList.toSet.map(tag => formatAnonAttributeTag(tag))
    val action = {
      DicomAnonymous.query.filter(da => da.attributeTag.inSet(tagSet))
    }

    // show database statement
    // action.result.statements.foreach(println)

    val list = Db.run(action.result)
    list
  }

  /**
    * Get previously anonymized values by tag for the given institution.
    */
  def getAttributesByTag(institutionPK: Long, tagList: Seq[AttributeTag]): Seq[DicomAnonymous] = {
    val tagSet = tagList.toSet.map(tag => formatAnonAttributeTag(tag))
    val action = {
      DicomAnonymous.query.filter(da => (da.institutionPK === institutionPK) && da.attributeTag.inSet(tagSet))
    }

    // show database statement
    // action.result.statements.foreach(println)

    val list = Db.run(action.result)
    list
  }

  /**
    * Get previously anonymized values by institution and attribute tag and value.
    *
    * Note that this must account for values that may or may not have an extra blank at the end.
    *
    * @param institutionPK Only for this institution.
    * @param attrList For this list of attributes.
    * @return Previously anonymized attributes.
    */
  def getByAttrAndValue(institutionPK: Long, attrList: Seq[Attribute]): Seq[DicomAnonymous] = {
    def tagSet = attrList.map(attr => formatAnonAttributeTag(attr.getTag)).toSet
    def valueSet = {
      val trimmed = attrList.map(_.getSingleStringValueOrEmptyString.trim).toSet.toSeq
      val extraBlank = trimmed.map(_ + " ")
      (trimmed ++ extraBlank).toSet
    }

    val action = DicomAnonymous.query.filter(da => (da.institutionPK === institutionPK) && da.value.inSet(valueSet) && da.attributeTag.inSet(tagSet))
    val list = Db.run(action.result)
    val pairSet = attrList.map(attr => (formatAnonAttributeTag(attr.getTag) + ":" + attr.getSingleStringValueOrEmptyString()).trim).toSet
    def daAsPair(da: DicomAnonymous) = (da.attributeTag + ":" + da.value).trim
    val inList = list.filter(da => pairSet.contains(daAsPair(da)))
    inList
  }

  def getLastPk(): Long = {
    val lastPk =
      try {
        val action = DicomAnonymous.query.map(da => da.dicomAnonymousPK).max
        val mx = Db.run(action.result)
        mx.get
      } catch {
        case _: Throwable => 0.toLong
      }
    lastPk
  }

  def getByPkLargerThan(pk: Long): Seq[DicomAnonymous] = {
    val action = DicomAnonymous.query.filter(da => da.dicomAnonymousPK > pk)
    val list = Db.run(action.result)
    list
  }

}
