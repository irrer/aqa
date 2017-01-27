package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node

case class LeafOffsetCorrection(
        leafOffsetCorrectionPK: Option[Long], // primary key
        outputPK: Long, // output primary key
        section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
        leafIndex: Int, // leaf number
        correction_mm: Double // maximum leaf gap
        ) {

    def insert: LeafOffsetCorrection = {
        val insertQuery = LeafOffsetCorrection.query returning LeafOffsetCorrection.query.map(_.leafOffsetCorrectionPK) into ((leafOffsetCorrection, leafOffsetCorrectionPK) => leafOffsetCorrection.copy(leafOffsetCorrectionPK = Some(leafOffsetCorrectionPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(LeafOffsetCorrection.query.insertOrUpdate(this))

    override def toString: String = (correction_mm.toString).trim
}

object LeafOffsetCorrection {
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
    }

    val query = TableQuery[LeafOffsetCorrectionTable]

    def get(leafOffsetCorrectionPK: Long): Option[LeafOffsetCorrection] = {
        val action = for {
            inst <- LeafOffsetCorrection.query if inst.leafOffsetCorrectionPK === leafOffsetCorrectionPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all leafOffsetCorrections.
     */
    def list = Db.run(query.result)

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

    def xmlToList(file: File): Seq[LeafOffsetCorrection] = {
        val doc = XML.loadFile(file)
        val outputPK = (doc \ "@outputPK").head.text.toLong
        def secNodeToLocList(sec: Node): Seq[LeafOffsetCorrection] = {
            val id = (sec \ "@id").head.text
            def leafNodeToLOC(leafNode: Node): LeafOffsetCorrection = {
                val leafIndex = (leafNode \ "@Leaf").head.text.toInt
                val correction_mm = leafNode.head.text.toDouble
                val loc = new LeafOffsetCorrection(None, outputPK, id, leafIndex, correction_mm)
                loc
            }
            val leafNodeList = (sec \ "LeafOffsetCorrection").map(leafNode => leafNodeToLOC(leafNode))
            leafNodeList
        }

        (doc \ "Section").map(sec => secNodeToLocList(sec)).flatten
    }

    def main(args: Array[String]): Unit = {
        val valid = Config.validate
        DbSetup.init
        val list = xmlToList(new File("results_LeafOffsetCorrection.xml"))
        list.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     correction_mm: " + loc.correction_mm))
        list.map(loc => loc.insertOrUpdate)
        //println("======== inst: " + get(5))
        //println("======== inst delete: " + delete(5))
    }
}
