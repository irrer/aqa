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

    def insertLeafList(xmlTag: String, insertIntoDb: (String, Int, Double) => Unit): Unit = {
      def leafNodeToLocList(leaf: Node): Unit = {
        val leafIndex = (leaf \ "leafIndex").head.text.toInt
        (leaf \ "Value").map(n => LOCXml.textToDouble(n.text)).zipWithIndex.foreach(di => insertIntoDb((di._2 + 1).toString, leafIndex, di._1))
      }

      val nodeList = (doc \ xmlTag \ "LeafList" \ "Leaf") ++ (doc \ xmlTag \ "Leaf")
      nodeList.foreach(leafNodeToLocList)
      logger.info("LOC data type: " + xmlTag + "   Number of data points: " + nodeList.size)
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
    //
    // Further note that some table names do not match the class names, as in:
    //   class: LOCRSquared is db table rSquared

    insertEPIDCenterCorrection()
    // @formatter:off
    
    insertLeafList( xmlTag = "LeafOffsetConstancy",
      (section: String, leafIndex: Int, value: Double) => { LeafOffsetCorrection(None, outputPK, section, leafIndex, value).insert })
    
    insertLeafList( xmlTag = "LeafTransmission",
      (section: String, leafIndex: Int, value: Double) => { LeafTransmission(None, outputPK, section, leafIndex, value).insert })
    
    insertLeafList( xmlTag = "LOCRSquared",
      (section: String, leafIndex: Int, value: Double) => { LOCRSquared(None, outputPK, section, leafIndex, value).insert })
    
    insertLeafList( xmlTag = "LOCDifferenceFromBaselineOpen",
      (section: String, leafIndex: Int, value: Double) => { DiffBaselineOpen(None, outputPK, section, leafIndex, value).insert }
    )
    
    insertLeafList( xmlTag = "LOCDifferenceFromBaselineTrans",
      (section: String, leafIndex: Int, value: Double) => { DiffBaselineTrans(None, outputPK, section, leafIndex, value).insert }
    )
    
    // @formatter:on
  }

  /*
  def main(args: Array[String]): Unit = {
    val xmlFile = new File("""D:\tmp\aqa\output.xml""")
    val doc = XML.loadFile(xmlFile, -1)
    LOCInsertIntoDb.insert(doc)
  }
   */
}
