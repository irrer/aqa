package org.aqa.db

import slick.driver.PostgresDriver.api._
import org.aqa.Logging._
import org.aqa.Config
import org.aqa.Util
import java.io.File
import scala.xml.XML
import scala.xml.Node
import scala.xml.Elem

case class LeafTransmission(
        leafTransmissionPK: Option[Long], // primary key
        outputPK: Long, // output primary key
        section: String, // arbitrary section name.  May be used to associate this section with input data such as UID
        leafIndex: Int, // leaf number
        transmission_mm: Double // maximum leaf gap
        ) {

    def insert: LeafTransmission = {
        val insertQuery = LeafTransmission.query returning LeafTransmission.query.map(_.leafTransmissionPK) into ((leafTransmission, leafTransmissionPK) => leafTransmission.copy(leafTransmissionPK = Some(leafTransmissionPK)))
        val action = insertQuery += this
        val result = Db.run(action)
        result
    }

    def insertOrUpdate = Db.run(LeafTransmission.query.insertOrUpdate(this))

    override def toString: String = (transmission_mm.toString).trim
}

object LeafTransmission extends ProcedureOutput {
    class LeafTransmissionTable(tag: Tag) extends Table[LeafTransmission](tag, "leafTransmission") {

        def leafTransmissionPK = column[Long]("leafTransmissionPK", O.PrimaryKey, O.AutoInc)
        def outputPK = column[Long]("outputPK")
        def section = column[String]("section")
        def leafIndex = column[Int]("leafIndex")
        def correction_mm = column[Double]("correction_mm")

        def * = (
            leafTransmissionPK.?,
            outputPK,
            section,
            leafIndex,
            correction_mm) <> ((LeafTransmission.apply _)tupled, LeafTransmission.unapply _)
    }

    val query = TableQuery[LeafTransmissionTable]

    override val topXmlLabel = "LeafTransmissionList"

    def get(leafTransmissionPK: Long): Option[LeafTransmission] = {
        val action = for {
            inst <- LeafTransmission.query if inst.leafTransmissionPK === leafTransmissionPK
        } yield (inst)
        val list = Db.run(action.result)
        if (list.isEmpty) None else Some(list.head)
    }

    /**
     * Get a list of all leafTransmissions.
     */
    def list = Db.run(query.result)

    def delete(leafTransmissionPK: Long): Int = {
        val q = query.filter(_.leafTransmissionPK === leafTransmissionPK)
        val action = q.delete
        Db.run(action)
    }

    def deleteByOutputPK(outputPK: Long): Int = {
        val q = query.filter(_.outputPK === outputPK)
        val action = q.delete
        Db.run(action)
    }

    def xmlToList(elem: Elem, outputPK: Long): Seq[LeafTransmission] = {
        def secNodeToLocList(sec: Node): Seq[LeafTransmission] = {
            val id = (sec \ "@id").head.text
            def leafNodeToLOC(leafNode: Node): LeafTransmission = {
                val leafIndex = (leafNode \ "@Leaf").head.text.toInt
                val correction_mm = leafNode.head.text.toDouble
                val loc = new LeafTransmission(None, outputPK, id, leafIndex, correction_mm)
                loc
            }
            val leafNodeList = (sec \ "LeafTransmission_pct").map(leafNode => leafNodeToLOC(leafNode))
            leafNodeList
        }

        (elem \ topXmlLabel).headOption match {
            case Some(node) => (node \ "Section").map(sec => secNodeToLocList(sec)).flatten
            case None => Seq[LeafTransmission]()
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
        xmlList.map(loc => println("    outputPK: " + loc.outputPK + "     section: " + loc.section + "     leafIndex: " + loc.leafIndex + "     correction_mm: " + loc.transmission_mm))
        xmlList.map(loc => loc.insertOrUpdate)
        println("LeafTransmission.main done")
    }
}
