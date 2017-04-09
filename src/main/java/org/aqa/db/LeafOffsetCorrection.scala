package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class LeafOffsetCorrection(
        leafOffsetCorrectionPK: Option[Long], // primary key
        outputPK: Long, // output primary key
        section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
        leafIndex: Int, // leaf number
        correction_mm: Double // maximum leaf gap
        ) {

    def insert: LeafOffsetCorrection = {
        val insertQuery = LeafOffsetCorrection.query returning LeafOffsetCorrection.query.map(_.leafOffsetCorrectionPK) into
            ((leafOffsetCorrection, leafOffsetCorrectionPK) => leafOffsetCorrection.copy(leafOffsetCorrectionPK = Some(leafOffsetCorrectionPK)))

        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(LeafOffsetCorrection.query.insertOrUpdate(this))

    override def toString: String = (correction_mm.toString).trim
}

object LeafOffsetCorrection extends ProcedureOutput {
    class LeafOffsetCorrectionTable(tag: Tag) extends Table[LeafOffsetCorrection](tag, "leafOffsetCorrection") {

        def leafOffsetCorrectionPK = column[Long]("leafOffsetCorrectionPK", O.PrimaryKey, O.AutoInc)
        def outputPK = column[Long]("outputPK")
        def section = column[String]("section")
        def leafIndex = column[Int]("leafIndex")
        def correction_mm = column[Double]("correction_mm")

        def * = (
            leafOffsetCorrectionPK.?,
            outputPK,
            section,
            leafIndex,
            correction_mm) <> ((LeafOffsetCorrection.apply _)tupled, LeafOffsetCorrection.unapply _)

        def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
        //def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)           TODO
    }

    val query = TableQuery[LeafOffsetCorrectionTable]

    override val topXmlLabel = "LeafOffsetCorrectionList"

    def get(leafOffsetCorrectionPK: Long): Option[LeafOffsetCorrection] = {
        val action = for {
            inst <- LeafOffsetCorrection.query if inst.leafOffsetCorrectionPK === leafOffsetCorrectionPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all leafOffsetCorrections for the given output
     */
    def getByOutput(outputPK: Long): Seq[LeafOffsetCorrection] = {
        val action = for {
            inst <- LeafOffsetCorrection.query if inst.outputPK === outputPK
        } yield (inst)
        val list = Db.run(action.result)
        list
    }

    def delete(leafOffsetCorrectionPK: Long): Int = {
        val q = query.filter(_.leafOffsetCorrectionPK === leafOffsetCorrectionPK)
        val action = q.delete
        Db.run(action)
    }

    def deleteByOutputPK(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }

    def xmlToList(elem: Elem, outputPK: Long): Seq[LeafOffsetCorrection] = {
        def secNodeToLocList(sec: Node): Seq[LeafOffsetCorrection] = {
            val id = (sec \ "@id").head.text
            def leafNodeToLOC(leafNode: Node): LeafOffsetCorrection = {
                val leafIndex = (leafNode \ "@Leaf").head.text.toInt
                val correction_mm = leafNode.head.text.toDouble
                val loc = new LeafOffsetCorrection(None, outputPK, id, leafIndex, correction_mm)
                loc
            }
            val leafNodeList = (sec \ "LeafOffsetCorrection_mm").map(leafNode => leafNodeToLOC(leafNode))
            leafNodeList
        }

        (elem \ topXmlLabel).headOption match {
            case Some(node) => (node \ "Section").map(sec => secNodeToLocList(sec)).flatten
            case None => Seq[LeafOffsetCorrection]()
        }
    }

    override def insert(elem: Elem, outputPK: Long): Int = {
        val toInsert = xmlToList(elem, outputPK)
        toInsert.map(t => t.insertOrUpdate)
        toInsert.size
    }

    /** For testing only. */
    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        val elem = XML.loadFile(new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490\output.xml"""))
        val xmlList = xmlToList(elem, 134)
        xmlList.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     correction_mm: " + loc.correction_mm))
        xmlList.map(loc => loc.insertOrUpdate)
        println("LeafOffsetCorrection.main done")
        //println("======== inst: " + get(5))
        //println("======== inst delete: " + delete(5))
    }
}
