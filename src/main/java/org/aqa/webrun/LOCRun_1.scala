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
import scala.xml.XML

object LOCRun_1 {
    val parametersFileName = "parameters.xml"
    val LOCRun_1PKTag = "LOCRun_1PK"

    def main(args: Array[String]): Unit = {
        println("Starting")
        val valid = Config.validate
        DbSetup.init
        val procedure = Procedure.get(2).get

        val lr = new LOCRun_1(procedure)
        val output = Output.get(55).get

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

    private def makeDisplay(output: Output, outputPK: Long): String = {

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

        def groupToDataString(group: Seq[LeafValue]): String = {
            /** Number of digits of precision to display. */
            def fmt(d: Double): String = d.formatted("%7.5e")
            val sorted = group.sortWith((a, b) => a.leafIndex < b.leafIndex)
            "            ['Position" + sorted.head.section + "'," + sorted.map(x => x.value).toSeq.map(d => fmt(d)).mkString(", ") + "]"
        }

        def leavesToString(leaves: Seq[Int]): String = {
            "            ['Leaf'," + leaves.mkString(", ") + "]"
        }

        val transData = LeafTransmission.getByOutput(outputPK).map(v => new LeafValue(v.section, v.leafIndex, v.transmission_fract))
        val transLeaves = transData.map(_.section).distinct.sorted

        val offsetDataText: String = {
            val data = LeafOffsetCorrection.getByOutput(outputPK).map(v => new LeafValue(v.section, v.leafIndex, v.correction_mm))
            val leaves = data.map(_.leafIndex).distinct.sorted
            val groupList = data.groupBy(_.section).map(lo => lo._2).toSeq.sortWith((a, b) => a.head.section > b.head.section)
            leavesToString(leaves) + ",\n" + groupList.map(g => groupToDataString(g)).mkString(",\n")
        }

        val transDataText: String = {
            val data = LeafTransmission.getByOutput(outputPK).map(v => new LeafValue(v.section, v.leafIndex, v.transmission_fract))
            val leaves = data.map(_.leafIndex).distinct.sorted
            val groupList = data.groupBy(_.section).map(lo => lo._2).toSeq.sortWith((a, b) => a.head.section > b.head.section)
            leavesToString(leaves) + ",\n" + groupList.map(g => groupToDataString(g)).mkString(",\n")
        }

        val linkToFiles: Elem = {
            val url = ViewOutput.path + "?outputPK=" + outputPK + "&summary=true"
            <a href={ url }>Files</a>
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
                """,
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
                    pattern : [ '#8888bb', '#9999cc', '#aaaadd', '#bbbbee', '#ddddff' ]
                }
            });

            var TransChart = c3.generate({
                data: {
                    x: 'Leaf',
                    columns: [
                        """ + transDataText +
                """,
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
                    pattern : [ '#88bb88', '#99cc99', '#aaddaa', '#bbeebb', '#ddffdd' ]
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
                        <div class="col-md-1"><em>Elapsed:</em>{ elapsed }</div>
                        <div class="col-md-1"><em>Analysis by:</em>{ userId }</div>
                        <div class="col-md-2"><em>Procedure:</em>{ procedureDesc }</div>
                        <div class="col-md-2">{ linkToFiles }</div>
                    </div>
                    <div class="row">
                        <h4>Leaf Offset in mm</h4>
                    </div>
                    <div class="row">
                        <div id="LocChart"></div>
                        <h4>Leaf Transmission Percent</h4>
                    </div>
                    <div class="row">
                        <div id="TransChart"></div>
                    </div>
                </div>
            }

            wrapBody(div, "LOC", None, true, Some(runScript))
        }

        make
    }

    private def insertIntoDatabase(outputPK: Long) = {
        val elem = XML.loadFile(new File(ProcedureOutputUtil.outputFileName))
        ProcedureOutputUtil.insertIntoDatabase(elem, Some(outputPK))
    }

    override def postPerform(activeProcess: ActiveProcess): Unit = {
        activeProcess.output.outputPK match {
            case Some(outputPK) => {
                val output = Output.get(outputPK).get
                insertIntoDatabase(outputPK)
                val content = makeDisplay(output, outputPK)
                val file = new File(activeProcess.output.dir, Output.displayFilePrefix + ".html")
                logInfo("Writing file " + file.getAbsolutePath)
                Util.writeFile(file, content)
            }
            case None => ;
        }
    }
}
