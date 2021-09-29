package org.aqa.webrun.LOC

import org.aqa.Logging
import org.aqa.db.DiffBaselineOpen
import org.aqa.db.DiffBaselineTrans
import org.aqa.db.EPIDCenterCorrection
import org.aqa.db.LOCRSquared
import org.aqa.db.LeafOffsetCorrection
import org.aqa.db.LeafTransmission
import org.aqa.webrun.LOCXml

import scala.xml.Elem
import scala.xml.Node

object LOCInsertIntoDb extends Logging {

  def insert(doc: Elem, outputPK: Long): Unit = {

    import scala.reflect.runtime.universe._

    def insertLeafList[T: TypeTag](xmlTag: String): Unit = {
      val typeName = typeOf[T].toString
      def constructAndInsert(outputPK: Long, section: String, leafIndex: Int, value: Double): Unit = {
        0 match {
          case _ if typeName.equals(typeOf[LeafOffsetCorrection].toString) => LeafOffsetCorrection(None, outputPK, section, leafIndex, value).insert
          case _ if typeName.equals(typeOf[LeafTransmission].toString)     => LeafTransmission(None, outputPK, section, leafIndex, value).insert
          case _ if typeName.equals(typeOf[LOCRSquared].toString)          => LOCRSquared(None, outputPK, section, leafIndex, value).insert
          case _ if typeName.equals(typeOf[DiffBaselineOpen].toString)     => DiffBaselineOpen(None, outputPK, section, leafIndex, value).insert
          case _ if typeName.equals(typeOf[DiffBaselineTrans].toString)    => DiffBaselineTrans(None, outputPK, section, leafIndex, value).insert
          case _                                                           => throw new RuntimeException("Unexpected LOC type: " + typeName)
        }
      }

      def leafNodeToLocList(leaf: Node): Unit = {
        val leafIndex = (leaf \ "leafIndex").head.text.toInt
        (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.foreach(di => constructAndInsert(outputPK, (di._2 + 1).toString, leafIndex, di._1))
      }

      val nodeList = (doc \ xmlTag \ "LeafList" \ "Leaf") ++ (doc \ xmlTag \ "Leaf")
      nodeList.foreach(leafNodeToLocList)
      logger.info("LOC data type: " + typeName + "   Number of data points: " + nodeList.size)
    }

    def insertEPIDCenterCorrection(): Unit = {
      def insert(node: Node): Unit = {
        val epidCenterCorrection = EPIDCenterCorrection(None, outputPK, node.text.toDouble)
        logger.info("Inserting LOC EPIDCenterCorrection: " + epidCenterCorrection)
        epidCenterCorrection.insert
      }
      (doc \ "EPIDCenterCorrection").foreach(insert)
    }

    // Note that names in XML do not match class names.  Most are just abbreviations,
    // but Correction vs Constancy is disconcerting.
    insertEPIDCenterCorrection()
    insertLeafList[LeafOffsetCorrection]("LeafOffsetConstancy")
    insertLeafList[LeafTransmission]("LeafTransmission")
    insertLeafList[LOCRSquared]("LOCRSquared")
    insertLeafList[DiffBaselineOpen]("LOCDifferenceFromBaselineOpen")
    insertLeafList[DiffBaselineTrans]("LOCDifferenceFromBaselineTrans")
  }

  /*
  def main(args: Array[String]): Unit = {
    val xmlFile = new File("""D:\tmp\aqa\output.xml""")
    val doc = XML.loadFile(xmlFile, -1)
    LOCInsertIntoDb.insert(doc)
  }
   */
}
