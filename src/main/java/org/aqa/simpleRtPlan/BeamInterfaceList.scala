package org.aqa.simpleRtPlan

import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.DicomFile
import org.aqa.Util
import org.aqa.web.WebUtil.StyleMapT
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.web.WebUtil.WebPlainText
import org.aqa.web.WebUtil.WebRow

import scala.xml.Elem

case class BeamInterfaceList(templateFiles: TemplateFiles) {

  /**
    * List of beam parameters.
    *
    * @return Parameters for all beams.
    */
  val beamList: Seq[BeamInterface] = {
    val rtplan = new DicomFile(templateFiles.ofModality("RTPLAN").head.file).attributeList.get


    val beamAlList = {
      // Types of delivery we are interested in.
      val DeliveryTypeList = Seq("TREATMENT", "SETUP")
      val list = DicomUtil.seqToAttr(rtplan, TagByName.BeamSequence)

      // Limit the beams shown to the types we are interested in.
      list.filter(b => DeliveryTypeList.contains(b.get(TagByName.TreatmentDeliveryType).getSingleStringValueOrEmptyString()))
    }
    beamAlList.map(beamAl => BeamInterface(rtplan, beamAl))
  }

  /**
    * Create a list of initial values.
    *
    * @return List fo initial values.
    */
  def beamInitialValueMap: ValueMapT = {
    val list = beamList.flatMap(bi => bi.initialValueMap())
    val valueMap = list.toMap
    valueMap
  }

  /**
    * Validate all values specified in the treatment beams.
    *
    * @param valueMap Values specified by user.
    * @return Empty list if all is ok, otherwise list of errors.
    */
  def validateBeamFields(valueMap: ValueMapT): StyleMapT = {
    val errorList = beamList.map(b => b.validateBeam(valueMap))
    errorList.flatten.toMap
  }

  /**
    * Make the list of rows to be displayed on the web form.
    *
    * @return List of rows.
    */
  def makeWebRows(): List[WebRow] = {

    /**
      * Make one horizontal row if the input fields.
      * @param rowIndex Index of row
      * @return One horizontal WebRow
      */
    def makeRow(rowIndex: Int): WebRow = {
      val name = beamList.head.colList(rowIndex)
      val header = new WebPlainText(label = "rowHeader" + rowIndex, showLabel = false, col = 1, offset = 0, html = _ => <b style="white-space: nowrap;">{name.name}</b>)
      val fieldList = beamList.map(beam => beam.colList(rowIndex).field)
      //Trace.trace(rowIndex + " WebRow " + name.name)
      (header +: fieldList).toList
    }

    // make each row
    val rowList = beamList.head.colList.indices.map(makeRow).toList
    rowList
  }

  def makeReviewTable(valueMap: ValueMapT): Elem = {
    def makeRow(rowIndex: Int): Elem = {
      val name = beamList.head.colList(rowIndex)
      val row = beamList.map(beam => {
        val value = valueMap.get(beam.colList(rowIndex).label) match {
          case Some(text) => text
          case _          => beam.colList(rowIndex).init()
        }
        if (rowIndex == 0)
          <td><b>{value}</b></td>
        else
          <td>{value}</td>
      })
      <tr><td><b>{beamList.head.colList(rowIndex).name}</b></td> {row}</tr>
    }

    val rowList = beamList.head.colList.indices.map(makeRow).toList

    <table class="table table-bordered">{rowList}</table>
  }

  def makeCsvSummary(valueMap: ValueMapT): String = {
    def makeRow(rowIndex: Int): String = {
      val row = beamList.map(beam => {
        val value = valueMap.get(beam.colList(rowIndex).label) match {
          case Some(text) => text
          case _          => beam.colList(rowIndex).init()
        }
        value
      })
      val list = (beamList.head.colList(rowIndex).name +: row)
      list.map(Util.textToCsv).mkString(",")
    }

    val rowList = beamList.head.colList.indices.map(makeRow).toList.mkString("\n")

    rowList
  }

}
