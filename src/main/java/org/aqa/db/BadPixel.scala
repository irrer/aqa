package org.aqa.db

import Db.driver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

/**
 * Describe a bad pixel found in a DICOM image.
 */
case class BadPixel(
  badPixelPK: Option[Long], // primary key
  outputPK: Long, // output primary key
  x: Int, // X coordinate
  y: Int, // Y coordinate
  SOPInstanceUID: String, // UID of DICOM image
  imageName: String, // name of image.  If applicable the beam name is used.
  pixelValues_csv: String // square array of integer pixel values in CSV (comma separated values) format with bad pixel in the center. Pixels that are beyond the edge of the image are marked as 'NA'
) {

  def insert: BadPixel = {
    val insertQuery = BadPixel.query returning BadPixel.query.map(_.badPixelPK) into
      ((badPixel, badPixelPK) => badPixel.copy(badPixelPK = Some(badPixelPK)))

    val action = insertQuery += this
    val result = Db.run(action)
    result
  }

  def insertOrUpdate = Db.run(BadPixel.query.insertOrUpdate(this))

  override def toString: String = {
    "    badPixelPK: " + badPixelPK + "\n" +
      "    outputPK: " + outputPK + "\n" +
      "    x: " + x + "\n" +
      "    y: " + y + "\n" +
      "    SOPInstanceUID: " + SOPInstanceUID + "\n" +
      "    imageName: " + imageName + "\n" +
      "    pixelValues_csv: " + pixelValues_csv + "\n"
  }
}

object BadPixel extends ProcedureOutput {
  class BadPixelTable(tag: Tag) extends Table[BadPixel](tag, "badPixel") {

    def badPixelPK = column[Long]("badPixelPK", O.PrimaryKey, O.AutoInc)
    def outputPK = column[Long]("outputPK")
    def x = column[Int]("x")
    def y = column[Int]("y")
    def SOPInstanceUID = column[String]("SOPInstanceUID")
    def imageName = column[String]("imageName")
    def pixelValues_csv = column[String]("pixelValues_csv")

    def * = (
      badPixelPK.?,
      outputPK,
      x,
      y,
      SOPInstanceUID,
      imageName,
      pixelValues_csv) <> ((BadPixel.apply _)tupled, BadPixel.unapply _)

    def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
  }

  /**
   * Size of region around bad pixel reported for comparison. Should be a small positive integer.
   * For example: A value of n equal 2 results in (n*2+1 = 5) would mean that a 5x5 region would
   * be put in the CSV with the bad pixel at the center.
   */
  val radius = 2

  val query = TableQuery[BadPixelTable]

  override val topXmlLabel = "BadPixel"

  def get(badPixelPK: Long): Option[BadPixel] = {
    val action = for {
      inst <- BadPixel.query if inst.badPixelPK === badPixelPK
    } yield (inst)
    Db.run(action.result).headOption
  }

  /**
   * Get a list of all rows for the given output
   */
  def getByOutput(outputPK: Long): Seq[BadPixel] = {
    val action = for {
      inst <- BadPixel.query if inst.outputPK === outputPK
    } yield (inst)
    Db.run(action.result)
  }

  def delete(badPixelPK: Long): Int = {
    val q = query.filter(_.badPixelPK === badPixelPK)
    val action = q.delete
    Db.run(action)
  }

  def deleteByOutputPK(outputPK: Long): Int = {
    val q = query.filter(_.outputPK === outputPK)
    val action = q.delete
    Db.run(action)
  }

  def insert(list: Seq[BadPixel]) = {
    val ops = list.map { imgId => BadPixel.query.insertOrUpdate(imgId) }
    Db.perform(ops)
  }

  override def insert(elem: Elem, outputPK: Long): Int = {
    ??? // TODO
  }

  def insertSeq(list: Seq[BadPixel]): Unit = {
    val ops = list.map { loc => BadPixel.query.insertOrUpdate(loc) }
    Db.perform(ops)
  }
}
