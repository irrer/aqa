package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging
import java.sql.Timestamp
import com.pixelmed.dicom.AttributeList
import org.aqa.Util

/**
 * Content in addition to the baseline value.  Usually used for storing binary content.  There may be multiple <code>BaselineContent</code> instances associated with a single <code>Baseline</code> instance.
 */
case class BaselineContent(
  baselineContentPK: Option[Long], // primary key
  baselinePK: Long, // reference to baseline
  content: Array[Byte] // Whatever content is to be associated with the baseline.  Frequently used for DICOM files.
) {

  def insert: BaselineContent = {
    val insertQuery = BaselineContent.query returning BaselineContent.query.map(_.baselineContentPK) into ((baselineContent, baselineContentPK) => baselineContent.copy(baselineContentPK = Some(baselineContentPK)))
    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BaselineContent.query.insertOrUpdate(this))
}

object BaselineContent extends Logging {

  class BaselineContentTable(tag: Tag) extends Table[BaselineContent](tag, "baselineContent") {

    def baselineContentPK = column[Long]("baselineContentPK", O.PrimaryKey, O.AutoInc)
    def baselinePK = column[Long]("baselinePK")
    def content = column[Array[Byte]]("content")

    def * = (
      baselineContentPK.?,
      baselinePK,
      content) <> ((BaselineContent.apply _)tupled, BaselineContent.unapply _)

    def baselineFK = foreignKey("baselinePK", baselinePK, Baseline.query)(_.baselinePK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  val query = TableQuery[BaselineContentTable]

  def get(baselineContentPK: Long): Option[BaselineContent] = {
    val action = for {
      baselineContent <- query if baselineContent.baselineContentPK === baselineContentPK
    } yield (baselineContent)
    val list = Db.run(action.result)
    if (list.isEmpty) None else Some(list.head)
  }

  def delete(baselineContentPK: Long): Int = {
    val q = query.filter(_.baselineContentPK === baselineContentPK)
    logger.info("deleting baselineContent " + baselineContentPK)
    val action = q.delete
    Db.run(action)
  }

  /**
   * Construct a baseline content object using an attribute list.
   */
  def makeBaselineContent(baselinePK: Long, attributeList: AttributeList): BaselineContent = {
    Util.dicomToBytes(attributeList) match {
      case Left(err) => {
        val msg = "Could not create BaselineContent for DICOM: " + err
        logger.warn(msg)
        throw new RuntimeException(msg)
      }
      case Right(bytes) => {
        new BaselineContent(None, baselinePK, bytes)
      }
    }
  }

}
