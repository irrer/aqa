package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem
import org.aqa.procedures.ProcedureOutput

case class DiffBaselineOpen(
        diffBaselineOpenPK: Option[Long], // primary key
        outputPK: Long, // output primary key
        section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
        leafIndex: Int, // leaf number
        diffBaselineOpen_mm: Double // difference from baseline open value in mm
        ) {

    def insert: DiffBaselineOpen = {
        val insertQuery = DiffBaselineOpen.query returning DiffBaselineOpen.query.map(_.diffBaselineOpenPK) into
            ((diffBaselineOpen, diffBaselineOpenPK) => diffBaselineOpen.copy(diffBaselineOpenPK = Some(diffBaselineOpenPK)))

        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(DiffBaselineOpen.query.insertOrUpdate(this))

    override def toString: String = (diffBaselineOpen_mm.toString).trim
}

object DiffBaselineOpen extends ProcedureOutput {
    class DiffBaselineOpenTable(tag: Tag) extends Table[DiffBaselineOpen](tag, "diffBaselineOpen") {

        def diffBaselineOpenPK = column[Long]("diffBaselineOpenPK", O.PrimaryKey, O.AutoInc)
        def outputPK = column[Long]("outputPK")
        def section = column[String]("section")
        def leafIndex = column[Int]("leafIndex")
        def diffBaselineOpen_mm = column[Double]("diffBaselineOpen_mm")

        def * = (
            diffBaselineOpenPK.?,
            outputPK,
            section,
            leafIndex,
            diffBaselineOpen_mm) <> ((DiffBaselineOpen.apply _)tupled, DiffBaselineOpen.unapply _)

        def outputFK = foreignKey("outputPK", outputPK, Output.query)(_.outputPK, onDelete = ForeignKeyAction.Cascade, onUpdate = ForeignKeyAction.Cascade)
        //def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id, onUpdate=ForeignKeyAction.Restrict, onDelete=ForeignKeyAction.Cascade)           TODO
    }

    val query = TableQuery[DiffBaselineOpenTable]

    override val topXmlLabel = "LOCDifferenceFromBaselineOpen"

    def get(diffBaselineOpenPK: Long): Option[DiffBaselineOpen] = {
        val action = for {
            inst <- DiffBaselineOpen.query if inst.diffBaselineOpenPK === diffBaselineOpenPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all diffBaselineOpens for the given output
     */
    def getByOutput(outputPK: Long): Seq[DiffBaselineOpen] = {
        val action = for {
            inst <- DiffBaselineOpen.query if inst.outputPK === outputPK
        } yield (inst)
        val list = Db.run(action.result)
        list
    }

    def delete(diffBaselineOpenPK: Long): Int = {
        val q = query.filter(_.diffBaselineOpenPK === diffBaselineOpenPK)
        val action = q.delete
        Db.run(action)
    }

    def deleteByOutputPK(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }

    private def xmlToList(elem: Elem, outputPK: Long): Seq[DiffBaselineOpen] = {
        def leafNodeToLocList(leaf: Node): Seq[DiffBaselineOpen] = {
            val leafIndex = (leaf \ "leafIndex").head.text.toInt
            (leaf \ "Value").map(n => n.text.toDouble).zipWithIndex.map(di => new DiffBaselineOpen(None, outputPK, (di._2 + 1).toString, leafIndex, di._1))
        }

        (elem \ topXmlLabel).headOption match {
            case Some(node) => (node \ "Leaf").map(leaf => leafNodeToLocList(leaf)).flatten
            case None => Seq[DiffBaselineOpen]()
        }
    }

    override def insert(elem: Elem, outputPK: Long): Int = {
        val toInsert = xmlToList(elem, outputPK)
        insertSeq(toInsert)
        toInsert.size
    }

    def insertSeq(list: Seq[DiffBaselineOpen]): Unit = {
        val ops = list.map { loc => DiffBaselineOpen.query.insertOrUpdate(loc) }
        Db.perform(ops)
    }

    /** For testing only. */
    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        System.exit(99)
        //val elem = XML.loadFile(new File("""D:\AQA_Data\data\Chicago_33\TB5x_1\WinstonLutz_1.0_1\2016-12-09T09-50-54-361_134\output_2016-12-09T09-50-54-490\output.xml"""))
        val elem = XML.loadFile(new File("""D:\tmp\aqa\tmp\output.xml"""))
        val xmlList = xmlToList(elem, 134)
        xmlList.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     diffBaselineOpen_mm: " + loc.diffBaselineOpen_mm))
        xmlList.map(loc => loc.insertOrUpdate)
        println("DiffBaselineOpen.main done")
        //println("======== inst: " + get(5))
        //println("======== inst delete: " + delete(5))
    }
}
