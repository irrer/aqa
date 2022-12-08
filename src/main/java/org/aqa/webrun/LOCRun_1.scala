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

package org.aqa.webrun

import org.restlet.Request
import org.restlet.Response
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging
import org.aqa.db.Machine
import edu.umro.ScalaUtil.Trace._
import java.io.File
import org.aqa.db.Procedure
import org.aqa.run.Run
import org.aqa.Util
import org.aqa.web.WebUtil
import org.aqa.db.CentralAxis
import org.aqa.db.Institution
import org.restlet.Restlet
import com.pixelmed.dicom.DicomFileUtilities
import com.pixelmed.dicom.TagFromName
import edu.umro.util.Utility
import com.pixelmed.dicom.AttributeList
import org.aqa.web.WebRunIndex
import org.aqa.run.PostProcess
import org.aqa.run.PostProcess
import org.aqa.run.PostProcess
import org.aqa.run.ActiveProcess
import org.aqa.db.LeafOffsetCorrection
import org.aqa.db.LeafTransmission
import scala.xml.Elem
import java.util.Date
import org.aqa.db.User
import org.aqa.db.Output
import org.aqa.db.DbSetup
import org.aqa.Config
import org.aqa.run.ProcedureStatus
import org.aqa.db.DataValidity
import java.sql.Timestamp
import org.aqa.db.Input
import org.aqa.web.ViewOutput
import org.aqa.db.EPIDCenterCorrection
import org.aqa.procedures.ProcedureOutputUtil
import java.io.File
import org.apache.poi.ss.usermodel.Workbook
import org.apache.poi.ss.usermodel.Sheet
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.ss.usermodel.Row
import edu.umro.MSOfficeUtil.Excel.ExcelUtil
import scala.xml.XML
import org.aqa.run.StdLogger

object LOCRun_1 {
  val parametersFileName = "parameters.xml"
  val LOCRun_1PKTag = "LOCRun_1PK"
  val spreadsheetHtmlFileName = "spreadsheet.html"
  val spreadsheetFileName = "spreadsheet.xlsx"

  def main(args: Array[String]): Unit = {
    println("Starting")

    if (true) {
      val dir = new File("""D:\tmp\aqa\tmp\mario\bad""")
      val locXml = new LOCXml(dir)
      println("locXml: " + locXml)
      println("Done")
      System.exit(99)
    }

    if (true) {
      val inFile = new File("""D:\AQA_Data\results\TBD_2\CHIC1_11\Leaf_Offset_and_Transmission_1.0.0_2\2017-05-16T12-16-11-212_118\output_2017-05-16T12-16-11-261\spreadsheet.xlsx""")
      val workbook = ExcelUtil.read(inFile).right.get
      val html = excelToHtml(workbook)
      val outFile = new File("""D:\AQA_Data\results\TBD_2\CHIC1_11\Leaf_Offset_and_Transmission_1.0.0_2\2017-05-16T12-16-11-212_118\output_2017-05-16T12-16-11-261\tabby.html""")
      Util.writeFile(outFile, html)
      println("Done")
      System.exit(99)
    }
    val valid = Config.validate
    DbSetup.init
    val procedure = Procedure.get(2).get

    val lr = new LOCRun_1(procedure)
    val output = Output.get(111).get

    val request = new Request
    request.setHostRef("https://aqa.org/view/ViewOutput?outputPK=126")
    val response = new Response(request)
    val ap = new ActiveProcess(output, null, null, null, response)

    lr.postPerform(ap)

    println("Done")
  }

}

/**
 * Run LOC code.
 */
class LOCRun_1(procedure: Procedure) extends WebRunProcedure(procedure) with PostProcess with Logging {

  /** Defines precision - Format to use when showing numbers. */
  val outputFormat = "%7.5e"

  private def okDbl(d: Double) = { (d < Double.MaxValue) && (d > Double.MinValue) }

  //def machineList() = ("-1", "None") +: Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

  private def makeButton(name: String, primary: Boolean, buttonType: ButtonType.Value): FormButton = {
    val action = procedure.webUrl + "?" + name + "=" + name
    new FormButton(name, 1, 0, SubUrl.run, action, buttonType)
  }

  private val runButton = makeButton("Run", true, ButtonType.BtnDefault)
  private val cancelButton = makeButton("Cancel", false, ButtonType.BtnDefault)

  private def form = new WebForm(procedure.webUrl, Some("LOC"), List(List(runButton, cancelButton)), 6)

  private def emptyForm(valueMap: ValueMapT, response: Response) = {
    form.setFormResponse(valueMap, styleNone, procedure.name, response, Status.SUCCESS_OK)
  }

  private case class RunRequirementsLOC(machine: Machine, sessionDir: File, alList: Seq[AttributeList]);

  private def validate(valueMap: ValueMapT): Either[StyleMapT, RunRequirementsLOC] = {
    lazy val alList = dicomFilesInSession(valueMap).map(df => df.attributeList).flatten

    // machines that DICOM files reference (based on device serial numbers)
    lazy val machList = alList.map(al => Machine.attributeListToMachine(al)).flatten.distinct

    // The machine to use
    lazy val mach = machList.headOption

    def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))
    logger.info("the machList.isEmpty: " + machList.isEmpty) // TODO rm

    sessionDir(valueMap) match {
      case Some(dir) if (!dir.isDirectory) => formErr("No files have been uploaded")
      case _ if (alList.isEmpty) => formErr("No DICOM files have been uploaded.")
      case _ if (machList.size > 1) => formErr("Files from more than one machine were found.  Click Cancel to start over.")
      case _ if (machList.isEmpty) => {
        logger.info("machList.isEmpty: " + machList.isEmpty) // TODO rm
        formErr("These files do not have a serial number of a known machine.  Click Cancel and use the baseline LOC upload procedure.")
      }
      case _ if (!LOCUploadBaseFiles_1.ensureBaseline(mach.get.machinePK.get)) => formErr("There are no baseline files for machine " + mach.get.id + ".  Click Cancel and use the baseline LOC upload procedure.")
      case Some(dir) => Right(new RunRequirementsLOC(mach.get, dir, alList))
    }
  }

  /**
   * Run the procedure.
   */
  private def run(valueMap: ValueMapT, request: Request, response: Response) = {
    validate(valueMap) match {
      case Right(runReq) => {
        val machPK = runReq.machine.machinePK.get

        val dtp = Util.dateTimeAndPatientIdFromDicom(runReq.sessionDir)
        Run.run(procedure, Machine.get(machPK).get, runReq.sessionDir, request, response, dtp.PatientID, dtp.dateTime, Some(this.asInstanceOf[PostProcess]))
      }
      case Left(errMap) => form.setFormResponse(valueMap, errMap, procedure.name, response, Status.CLIENT_ERROR_BAD_REQUEST)
    }
  }

  /**
   * Cancel the procedure.  Remove files and redirect to procedure list.
   */
  private def cancel(valueMap: ValueMapT, response: Response) = {
    sessionDir(valueMap) match {
      case Some(dir) => Utility.deleteFileTree(dir)
      case _ => ;
    }
    WebRunIndex.redirect(response)
  }

  private def buttonIs(valueMap: ValueMapT, button: FormButton): Boolean = {
    val value = valueMap.get(button.label)
    value.isDefined && value.get.toString.equals(button.label)
  }

  /**
   * Main entry point of web interface.
   */
  override def handle(request: Request, response: Response): Unit = {
    super.handle(request, response)

    val valueMap: ValueMapT = emptyValueMap ++ getValueMap(request)

    try {
      0 match {
        //case _ if (!sessionDefined(valueMap)) => redirectWithNewSession(response);
        case _ if buttonIs(valueMap, cancelButton) => cancel(valueMap, response)
        case _ if buttonIs(valueMap, runButton) => run(valueMap, request, response)
        case _ => emptyForm(valueMap, response)
      }
    } catch {
      case t: Throwable => {
        internalFailure(response, "Unexpected failure: " + fmtEx(t))
      }
    }
  }

  case class FileWorkbook(file: File, workbook: Workbook);

  private def makeDisplay(output: Output, outputPK: Long, locXml: LOCXml, fileWorkbookList: Seq[FileWorkbook]): String = {

    val machine = for (machPK <- output.machinePK; mach <- Machine.get(machPK)) yield (mach)

    val machineId = machine match {
      case Some(mach) => mach.id
      case _ => ""
    }

    val numberOfBadValues = {
      val all = locXml.LeafOffsetConstancyMean ++
        locXml.LeafOffsetConstancyRange ++
        locXml.LeafOffsetConstancySectionMean ++
        locXml.LeafOffsetConstancySectionSTD ++
        locXml.LeafOffsetConstancySectionCoeffOfVar ++
        locXml.LeafOffsetConstancySectionRange ++
        locXml.LeafOffsetTransmissionMean ++
        locXml.LeafOffsetTransmissionSectionMean ++
        locXml.LeafOffsetTransmissionSectionSTD ++
        locXml.LeafOffsetTransmissionSectionCoeffOfVar ++
        locXml.LeafOffsetTransmissionSectionRange ++
        locXml.LeafOffsetTransmissionValue.flatten ++
        locXml.LOCRSquared.flatten ++
        locXml.LOCDifferenceFromBaselineOpen.flatten ++
        locXml.LOCDifferenceFromBaselineTrans.flatten ++
        locXml.LeafOffsetConstancyValue.flatten

      all.filter(d => !okDbl(d)).size
    }

    val institution = for (mach <- machine; inst <- Institution.get(mach.institutionPK)) yield (inst)

    val institutionName = institution match {
      case Some(inst) => inst.name
      case _ => ""
    }

    val epidCenterCorrection: String = {
      EPIDCenterCorrection.getByOutput(outputPK).headOption match {
        case Some(ecc) => ecc.epidCenterCorrection_mm.formatted("%6.3f")
        case _ => "not available"
      }
    }

    val user = for (userPK <- output.userPK; u <- User.get(userPK)) yield (u)

    val userId = user match {
      case Some(u) => u.id
      case _ => ""
    }

    val elapsed: String = {
      val fin = output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _ => System.currentTimeMillis
      }
      val elapsed = fin - output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val analysisDate: String = {
      val date = output.analysisDate match {
        case Some(d) => d
        case _ => output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    def dateToString(date: Option[Date]): String = {
      date match {
        case Some(date) => Util.timeHumanFriendly(date)
        case _ => "unknown"
      }
    }

    val procedureDesc: String = {
      Procedure.get(output.procedurePK) match {
        case Some(proc) =>
          proc.name + " : " + proc.version
        case _ => ""
      }
    }

    case class LeafValue(section: String, leafIndex: Int, value: Double);

    def fmt(d: Double): String = d.formatted("%7.5e")

    def groupToDataString(group: Seq[LeafValue]): String = {
      /** Number of digits of precision to display. */
      val sorted = group.sortWith((a, b) => a.leafIndex < b.leafIndex)
      "            ['Position" + sorted.head.section + "'," + sorted.map(x => x.value).toSeq.map(d => fmt(d)).mkString(", ") + "]"
    }

    def leavesToString(leaves: Seq[Int]): String = {
      "            ['Leaf'," + leaves.mkString(", ") + "]"
    }

    val transData = LeafTransmission.getByOutput(outputPK).map(v => new LeafValue(v.section, v.leafIndex, v.transmission_fract))
    val transLeaves = transData.map(_.section).distinct.sorted
    val leavesText = locXml.leafIndexList.distinct.sorted.mkString("            ['Leaf', ", ", ", "]")

    def twoD2Text(data: Seq[Seq[Double]]): String = {
      def sec2String(s: Int): String = {
        val textNums = data.map(v => v(s)).map(d => fmt(d))
        textNums.mkString("            ['Position" + (s + 1) + "', ", ", ", "]")
      }
      val values = (0 until locXml.sections).map(s => sec2String(s)).reverse.mkString(",\n")
      values
    }

    def oneD2Text(name: String, data: Seq[Double]): String = {
      "            ['" + name + "', " + data.map(m => fmt(m)).mkString(", ") + " ]"
    }

    def fixSeq(seq: Seq[Double]): Seq[Double] = {
      val good = seq.filter(d => okDbl(d))
      val avg = good.sum / good.size
      seq.map(d => if (okDbl(d)) d else avg)
    }

    def fixSeqSeq(seqSeq: Seq[Seq[Double]]): Seq[Seq[Double]] = seqSeq.map(seq => fixSeq(seq))

    val offsetDataText: String = {
      val values = twoD2Text(fixSeqSeq(locXml.LeafOffsetConstancyValue))
      val mean = oneD2Text("Mean", fixSeq(locXml.LeafOffsetConstancyMean)) // "       ['Mean', " + locXml.LeafOffsetConstancyMean.map(m => fmt(m)).mkString(", ") + " ],\n"
      val range = oneD2Text("Range", fixSeq(locXml.LeafOffsetConstancyRange)) // "  ['Range', " + locXml.LeafOffsetConstancyRange.map(m => fmt(m)).mkString(", ") + " ]\n"
      Seq(leavesText, values, mean, range).mkString(",\n")
    }

    val transDataText: String = {
      val values = twoD2Text(fixSeqSeq(locXml.LeafOffsetTransmissionValue))
      val mean = oneD2Text("Mean", fixSeq(locXml.LeafOffsetTransmissionMean)) // "   ['Mean', " + locXml.LeafOffsetConstancyMean.map(m => fmt(m)).mkString(", ") + " ],\n"
      Seq(leavesText, values, mean).mkString(",\n")
    }

    val rSquaredText: String = {
      val values = twoD2Text(fixSeqSeq(locXml.LOCRSquared))
      Seq(leavesText, values).mkString(",\n")
    }

    val differenceFromBaselineOpenText: String = {
      val values = twoD2Text(fixSeqSeq(locXml.LOCDifferenceFromBaselineOpen))
      Seq(leavesText, values).mkString(",\n")
    }

    val differenceFromBaselineTransText: String = {
      val values = twoD2Text(fixSeqSeq(locXml.LOCDifferenceFromBaselineTrans))
      Seq(leavesText, values).mkString(",\n")
    }

    val linkToFiles: Elem = {
      val url = ViewOutput.path + "?outputPK=" + outputPK + "&summary=true"
      <a href={ url }>Files</a>
    }

    def spreadSheetLinks(fileWorkbookList: Seq[FileWorkbook]): IndexedSeq[Elem] = {
      val spaces = nbsp + " " + nbsp + " " + nbsp + " " + nbsp + " " + nbsp

      def links(fs: FileWorkbook) = {
        val base = Util.fileBaseName(fs.file)
        Seq(
          { <div><a href={ base + ".html" } title="View HTML version of spreadsheet">View { base }</a>{ spaces }</div> },
          { <div><a href={ fs.file.getName } title="Download spreadsheet">Download { base }</a>{ spaces }</div> })
      }

      if (fileWorkbookList.isEmpty) {
        IndexedSeq({ <div title="There were not spreadsheets found.  Check the log for possible errors.">No Spreadsheets</div> })
      } else fileWorkbookList.map(fw => links(fw)).flatten.toIndexedSeq
    }

    val viewLog: Elem = {
      <a href={ StdLogger.LOG_TEXT_FILE_NAME }>View Log</a>
    }

    val viewXml: Elem = {
      <a href={ ProcedureOutputUtil.outputFileName }>View XML</a>
    }

    /**
     * Javascript to display the graphs.
     */
    def runScript = {
      """
            <script>
            var LocChart = c3.generate({
                data: {
                    x: 'Leaf',
                    columns: [
""" + offsetDataText +
        """
                    ]
                },
                bindto : '#LocChart',
                axis: {
                    x: {
                        label: 'Leaf',
                    },
                    y: {
                        label: 'Offset in mm',
                        tick: {
                            format: d3.format('.4f')
                        }
                    }
                },
                color : {
                    pattern : [ '#6688bb', '#7788bb', '#8888bb', '#9999cc', '#aaaadd', '#f5b800', '#e49595' ]
                }
            });

            var TransChart = c3.generate({
                data: {
                    x: 'Leaf',
                    columns: [
""" + transDataText +
        """
                    ]
                },
                bindto : '#TransChart',
                axis: {
                    x: {
                        label: 'Leaf',
                    },
                    y: {
                        label: 'Leaf Transmission Fraction'
                    }
                },
                color : {
                    pattern : [ '#66bb88', '#77bb88', '#88bb88', '#99cc99', '#aaddaa', '#f5b800' ]
                }
            });

            var RSquaredChart = c3.generate({
                data: {
                    x: 'Leaf',
                    columns: [
""" + rSquaredText +
        """
                    ]
                },
                bindto : '#RSquaredChart',
                axis: {
                    x: {
                        label: 'Leaf',
                    },
                    y: {
                        label: 'R Squared'
                    }
                },
                color : {
                    pattern : [ '#66bb88', '#77bb88', '#88bb88', '#99cc99', '#aaddaa' ]
                }
            });

            var DifferenceFromBaselineOpenChart = c3.generate({
                data: {
                    x: 'Leaf',
                    columns: [
""" + differenceFromBaselineOpenText +
        """
                    ]
                },
                bindto : '#DifferenceFromBaselineOpenChart',
                axis: {
                    x: {
                        label: 'Leaf',
                    },
                    y: {
                        label: 'Difference From Baseline Open'
                    }
                },
                color : {
                    pattern : [ '#66bb88', '#77bb88', '#88bb88', '#99cc99', '#aaddaa' ]
                }
            });

            var DifferenceFromBaselineTransChart = c3.generate({
                data: {
                    x: 'Leaf',
                    columns: [
""" + differenceFromBaselineTransText +
        """
                    ]
                },
                bindto : '#DifferenceFromBaselineTrans',
                axis: {
                    x: {
                        label: 'Leaf',
                    },
                    y: {
                        label: 'Difference From Baseline Transmission'
                    }
                },
                color : {
                    pattern : [ '#66bb88', '#77bb88', '#88bb88', '#99cc99', '#aaddaa' ]
                }
            });

            </script>
"""
    }

    def make: String = {
      def wrap(col: Int, elem: Elem): Elem = {
        <div class={ "col-md-" + col }>{ elem }</div>
      }

      def wrap2(col: Int, name: String, value: String): Elem = {
        <div class={ "col-md-" + col }><em>{ name }:</em><br/>{ value }</div>
      }

      def wrap2Anon(col: Int, name: String, value: String): Elem = {
        <div class={ "col-md-" + col }><em>{ name }:</em><br/><span aqaalias="">{ value }</span></div>
      }

      val div = {
        <div class="row col-md-10 col-md-offset-1">
          <div class="row">
            <div class="col-md-1" title="Leaf Offset Constancy and Transmission"><h2>LOC</h2></div>
            <div class="col-md-2 col-md-offset-1" title="Machine"><h2 aqaalias="">{ machineId }</h2></div>
            <div class="col-md-3 col-md-offset-1">EPID Center Correction in mm: { epidCenterCorrection }</div>
          </div>
          <div class="row" style="margin:20px;">
            { wrap2(1, "Institution", institutionName) }
            { wrap2(2, "Data Acquisition", dateToString(output.dataDate)) }
            { wrap2(2, "Analysis", analysisDate) }
            { wrap2Anon(1, "Analysis by", userId) }
            { wrap2Anon(1, "Elapsed", elapsed) }
            { wrap2(3, "Procedure", procedureDesc) }
          </div>
          <div class="row" style="margin:20px;">
            { spreadSheetLinks(fileWorkbookList).map(e => { wrap(3, e) }) }
          </div>
          <div class="row" style="margin:20px;">
            { wrap(2, linkToFiles) }
            { wrap(2, viewLog) }
            { wrap(2, viewXml) }
          </div>
          <div class="row">
            {
              if (numberOfBadValues > 0) {
                <div style="color:red">
                  <h3>Caution: There were { numberOfBadValues } invalid values in the results</h3>
                  <p>
                    Invalid values are caused by unchecked arithmetic calculations, such as
                    division by zero.  Examples are<b>NaN</b>
                    and<b>Infinity</b>
                  </p>
                  <p>
                    The values do not show up in the graphs below because they have been replaced by
                    the average of the others.  This is done so the graphs will still be of some use.
                  </p>
                  <p>
                    Download or view the online spreadsheet to determine the precise location where they occurred.
                    They will be displayed as <b>#NUM!</b>.
                  </p>
                </div>
              }
            }
          </div>
          <div class="row">
            <h4>Leaf Offset in mm</h4>
          </div>
          <div class="row">
            <div id="LocChart"></div>
          </div>
          <div class="row">
            <h4>Leaf Transmission Fraction</h4>
          </div>
          <div class="row">
            <div id="TransChart"></div>
          </div>
          <div class="row">
            <h4>R<sup>2</sup></h4>
          </div>
          <div class="row">
            <div id="RSquaredChart">aaaaa</div>
          </div>
          <div class="row">
            <h4>Difference from Baseline Open</h4>
          </div>
          <div class="row">
            <div id="DifferenceFromBaselineOpenChart">bbbbb</div>
          </div>
          <div class="row">
            <h4>Difference from Baseline Transmission</h4>
          </div>
          <div class="row">
            <div id="DifferenceFromBaselineTrans">ccccc</div>
          </div>
        </div>
      }

      wrapBody(div, "LOC", None, true, Some(runScript))
    }
    logger.info("Making LOC display for output " + outputPK)
    make
  }

  private def insertIntoDatabase(dir: File, outputPK: Long) = {
    val elem = XML.loadFile(new File(dir, ProcedureOutputUtil.outputFileName))
    ProcedureOutputUtil.insertIntoDatabase(elem, Some(outputPK))
  }

  private def getExcelWorkbookList(dir: File): Seq[FileWorkbook] = {
    val fileList = dir.listFiles.filter { f => f.getName.toLowerCase.contains(".xls") }
    logger.info("Number Excel spreadsheets found: " + fileList.size + fileList.map(f => "\n    " + f.getAbsolutePath).mkString)
    fileList.map(f => (f, ExcelUtil.read(f))).filter { fWb => fWb._2.isRight }.map(fWb => (fWb._1, fWb._2.right.get)).toSeq.map(fWb => new FileWorkbook(fWb._1, fWb._2))
  }

  private def makeSpreadsheet(dir: File, locXml: LOCXml, response: Response): Unit = {
    try {
      (new LOCSpreadsheet(dir, locXml, response)).write
    } catch {
      case t: Throwable => logger.warn("Failed to make spreadsheet: " + fmtEx(t))
    }
  }

  def excelToHtml(file: File, workbook: Workbook) = {
    try {
      val html = WebUtil.excelToHtml(workbook)
      val htmlFile = new File(file.getParentFile, Util.fileBaseName(file) + ".html")
      logger.info("Writing html version of spreadsheet to " + htmlFile.getAbsolutePath)
      Util.writeFile(htmlFile, html)
    } catch {
      case t: Throwable => logger.warn("Unable to write workbook for file " + file.getAbsolutePath + " : " + fmtEx(t))
    }
  }

  override def postPerform(activeProcess: ActiveProcess): Unit = {
    try {
      activeProcess.output.outputPK match {
        case Some(outputPK) => {
          logger.info("Starting post-processing")
          val output = Output.get(outputPK).get
          logger.info("Extracting output from XML file in " + output.dir.getAbsolutePath)
          val locXml = new LOCXml(output.dir)
          logger.info("Inserting into database")
          insertIntoDatabase(activeProcess.output.dir, outputPK)
          logger.info("Creating spreadsheet")
          makeSpreadsheet(activeProcess.output.dir, locXml, activeProcess.response)
          val excelWorkbookList = getExcelWorkbookList(activeProcess.output.dir)
          excelWorkbookList.map(fWb => excelToHtml(fWb.file, fWb.workbook))
          logger.info("Finished spreadsheets")
          val file = new File(activeProcess.output.dir, Output.displayFilePrefix + ".html")
          logger.info("Creating content for file " + file.getAbsolutePath)
          val content = makeDisplay(output, outputPK, locXml, excelWorkbookList)
          logger.info("Writing file " + file.getAbsolutePath)
          Util.writeFile(file, content)
          logger.info("Finished post-processing")
        }
        case None => ;
      }
    } catch {
      case t: Throwable => {
        logger.warn("Unexpected error in LOC postPerform: " + fmtEx(t))
      }
    }
  }
}
