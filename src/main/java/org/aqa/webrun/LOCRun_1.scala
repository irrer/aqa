package org.aqa.webrun

import org.restlet.Request
import org.restlet.Response
import slick.driver.PostgresDriver.api._
import play.api._
import play.api.libs.concurrent.Execution.Implicits._
import org.restlet.data.Status
import org.aqa.web.WebUtil._
import org.aqa.Logging._
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

    def main(args: Array[String]): Unit = {
        println("Starting")
        val valid = Config.validate
        DbSetup.init
        val procedure = Procedure.get(2).get

        val lr = new LOCRun_1(procedure)
        val output = Output.get(111).get

        val ap = new ActiveProcess(output, null, null, null)

        lr.postPerform(ap)

        println("Done")
    }

}

/**
 * Run LOC code.
 */
class LOCRun_1(procedure: Procedure) extends WebRunProcedure(procedure) with PostProcess {

    /** Defines precision - Format to use when showing numbers. */
    val outputFormat = "%7.5e"

    def machineList() = ("-1", "None") +: Machine.list.toList.map(m => (m.machinePK.get.toString, m.id))

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

    private case class RunRequirements(machine: Machine, sessionDir: File, alList: Seq[AttributeList]);

    private def validate(valueMap: ValueMapT): Either[StyleMapT, RunRequirements] = {
        lazy val alList = attributeListsInSession(valueMap)

        // machines that DICOM files reference (based on device serial numbers)
        lazy val machList = alList.map(al => attributeListToMachine(al)).flatten.distinct

        // The machine to use
        lazy val mach = machList.headOption

        def formErr(msg: String) = Left(Error.make(form.uploadFileInput.get, msg))

        sessionDir(valueMap) match {
            case Some(dir) if (!dir.isDirectory) => formErr("No files have been uploaded")
            case _ if (alList.isEmpty) => formErr("No DICOM files have been uploaded.")
            case _ if (machList.size > 1) => formErr("Files from more than one machine were found.  Click Cancel to start over.")
            case _ if (machList.isEmpty) => formErr("These files do not have a serial number of a known machine.  Click Cancel and use the baseline LOC upload procedure.")
            case Some(dir) => Right(new RunRequirements(mach.get, dir, alList))
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
        }
        catch {
            case t: Throwable => {
                internalFailure(response, "Unexpected failure: " + fmtEx(t))
            }
        }
    }

    private def makeDisplay(output: Output, outputPK: Long, locXml: LOCXml): String = {

        val machine = for (machPK <- output.machinePK; mach <- Machine.get(machPK)) yield (mach)

        val machineId = machine match {
            case Some(mach) => mach.id
            case _ => ""
        }

        val institution = for (mach <- machine; inst <- Institution.get(mach.institutionPK)) yield (inst)

        val institutionName = institution match {
            case Some(inst) => inst.name
            case _ => ""
        }

        val epidCenterCorrection: String = {
            EPIDCenterCorrection.getByOutput(outputPK).headOption match {
                case Some(ecc) => ecc.epidCenterCorrection_mm.formatted("%8.6e")
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

        //  import         org.apache.

        //  org.apache.commons.math4.util.Precision.
        //   org.apache.commons.ma
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

        val offsetDataText: String = {
            val values = twoD2Text(locXml.LeafOffsetConstancyValue)
            val mean = oneD2Text("Mean", locXml.LeafOffsetConstancyMean) // "            ['Mean', " + locXml.LeafOffsetConstancyMean.map(m => fmt(m)).mkString(", ") + " ],\n"
            val range = oneD2Text("Range", locXml.LeafOffsetConstancyRange) // "            ['Range', " + locXml.LeafOffsetConstancyRange.map(m => fmt(m)).mkString(", ") + " ]\n"
            Seq(leavesText, values, mean, range).mkString(",\n")
        }

        val transDataText: String = {
            val values = twoD2Text(locXml.LeafOffsetTransmissionValue)
            val mean = oneD2Text("Mean", locXml.LeafOffsetTransmissionMean) // "            ['Mean', " + locXml.LeafOffsetConstancyMean.map(m => fmt(m)).mkString(", ") + " ],\n"
            Seq(leavesText, values, mean).mkString(",\n")
        }

        val rSquaredText: String = {
            val values = twoD2Text(locXml.LOCRSquared)
            Seq(leavesText, values).mkString(",\n")
        }

        val differenceFromBaselineOpenText: String = {
            val values = twoD2Text(locXml.LOCDifferenceFromBaselineOpen)
            Seq(leavesText, values).mkString(",\n")
        }

        val differenceFromBaselineTransText: String = {
            val values = twoD2Text(locXml.LOCDifferenceFromBaselineTrans)
            Seq(leavesText, values).mkString(",\n")
        }

        val linkToFiles: Elem = {
            val url = ViewOutput.path + "?outputPK=" + outputPK + "&summary=true"
            val xlsFile: Elem = {
                val list = output.dir.list.filter(n => n.toLowerCase.contains(".xls"))
                list.headOption match {
                    case Some(name) => <a href={ name }>Download Spreadsheet</a>
                    case _ => <div>No Spreadsheet found</div>
                }
            }
            <a href={ url }>Files</a>
        }

        val viewSpreadsheet: Elem = {
            <a href={ LOCRun_1.spreadsheetHtmlFileName }>View Spreadsheet</a>
        }

        val downloadSpreadsheet: Elem = {
            val list = output.dir.list.filter(n => n.toLowerCase.contains(".xls"))
            list.headOption match {
                case Some(name) => <a href={ name }>Download Spreadsheet</a>
                case _ => <div>No Spreadsheet found</div>
            }
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
            val div = {
                <div class="row col-md-10 col-md-offset-1">
                    <div class="row">
                        <div class="col-md-1" title="Leaf Offset Constancy and Transmission"><h2>LOC</h2></div>
                        <div class="col-md-2 col-md-offset-1" title="Machine"><h2>{ machineId }</h2></div>
                        <div class="col-md-2 col-md-offset-1">EPID Center Correction in mm: { epidCenterCorrection }</div>
                    </div>
                    <div class="row" style="margin:20px;">
                        <div class="col-md-1"><em>Institution:</em>{ institutionName }</div>
                        <div class="col-md-2"><em>Data Acquisition:</em><br/>{ dateToString(output.dataDate) }</div>
                        <div class="col-md-2"><em>Analysis:</em><br/>{ analysisDate }</div>
                        <div class="col-md-1"><em>Analysis by:</em>{ userId }</div>
                        <div class="col-md-1"><em>Elapsed:</em>{ elapsed }</div>
                        <div class="col-md-2"><em>Procedure:</em>{ procedureDesc }</div>
                    </div>
                    <div class="row" style="margin:20px;">
                        <div class="col-md-2">{ linkToFiles }</div>
                        <div class="col-md-2">{ viewSpreadsheet }</div>
                        <div class="col-md-2">{ downloadSpreadsheet }</div>
                        <div class="col-md-2">{ viewLog }</div>
                        <div class="col-md-2">{ viewXml }</div>
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

        make
    }

    private def insertIntoDatabase(dir: File, outputPK: Long) = {
        val elem = XML.loadFile(new File(dir, ProcedureOutputUtil.outputFileName))
        ProcedureOutputUtil.insertIntoDatabase(elem, Some(outputPK))
    }

    private def getExcelFile(dir: File): Option[Workbook] = {
        val fileList = dir.listFiles.filter { f => f.getName.toLowerCase.contains(".xls") }
        val workbookList = fileList.map(af => ExcelUtil.read(af)).filter { wb => wb.isRight }.map(w => w.right.get)
        workbookList.headOption
    }

    private def makeSpreadsheet(dir: File, locXml: LOCXml): Unit = {
        // TODO
    }

    private def excelToXml(dir: File) = {
        getExcelFile(dir) match {
            case Some(workbook) => {
                val html = WebUtil.excelToHtml(workbook)
                val htmlFile = new File(dir, LOCRun_1.spreadsheetHtmlFileName)
                logInfo("Writing html version of spreadsheet to " + htmlFile.getAbsolutePath)
                Util.writeFile(htmlFile, html)
            }
            case _ => logWarning("No Excel spreadsheet found.")
        }

    }

    override def postPerform(activeProcess: ActiveProcess): Unit = {
        activeProcess.output.outputPK match {
            case Some(outputPK) => {
                val output = Output.get(outputPK).get
                val locXml = new LOCXml(output.dir)
                insertIntoDatabase(activeProcess.output.dir, outputPK)
                makeSpreadsheet(activeProcess.output.dir, locXml)
                excelToXml(activeProcess.output.dir)
                val content = makeDisplay(output, outputPK, locXml)
                val file = new File(activeProcess.output.dir, Output.displayFilePrefix + ".html")
                logInfo("Writing file " + file.getAbsolutePath)
                Util.writeFile(file, content)
            }
            case None => ;
        }
    }
}
