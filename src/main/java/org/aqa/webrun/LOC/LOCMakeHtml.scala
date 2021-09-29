package org.aqa.webrun.LOC

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.EPIDCenterCorrection
import org.aqa.db.Institution
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.db.Procedure
import org.aqa.db.User
import org.aqa.procedures.ProcedureOutputUtil
import org.aqa.run.StdLogger
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil._

import java.util.Date
import scala.xml.Elem

object LOCMakeHtml extends Logging {

  private def okDbl(d: Double) = { (d < Double.MaxValue) && (d > Double.MinValue) }

  def makeDisplay(output: Output, locXml: LOCToXml, fileWorkbookList: Seq[LOCFileWorkbook]): String = {

    val machine = for (machPK <- output.machinePK; mach <- Machine.get(machPK)) yield mach

    val machineId = machine match {
      case Some(mach) => mach.id
      case _          => ""
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

      all.count(d => !okDbl(d))
    }

    val institution = for (mach <- machine; inst <- Institution.get(mach.institutionPK)) yield inst

    val institutionName = institution match {
      case Some(inst) => inst.name
      case _          => ""
    }

    val epidCenterCorrection: String = {
      EPIDCenterCorrection.getByOutput(output.outputPK.get).headOption match {
        case Some(ecc) => ecc.epidCenterCorrection_mm.formatted("%6.3f")
        case _         => "not available"
      }
    }

    val user = for (userPK <- output.userPK; u <- User.get(userPK)) yield u

    val userId = user match {
      case Some(u) => u.id
      case _       => ""
    }

    val elapsed: String = {
      val fin = output.finishDate match {
        case Some(finDate) => finDate.getTime
        case _             => System.currentTimeMillis
      }
      val elapsed = fin - output.startDate.getTime
      Util.elapsedTimeHumanFriendly(elapsed)
    }

    val analysisDate: String = {
      val date = output.analysisDate match {
        case Some(d) => d
        case _       => output.startDate
      }
      Util.timeHumanFriendly(date)
    }

    def dateToString(date: Option[Date]): String = {
      date match {
        case Some(date) => Util.timeHumanFriendly(date)
        case _          => "unknown"
      }
    }

    val procedureDesc: String = {
      Procedure.get(output.procedurePK) match {
        case Some(proc) =>
          proc.name + " : " + proc.version
        case _ => ""
      }
    }

    case class LeafValue(section: String, leafIndex: Int, value: Double) {}

    def fmt(d: Double): String = d.formatted("%7.5e")

    /*
    def groupToDataString(group: Seq[LeafValue]): String = {

      /** Number of digits of precision to display. */
      val sorted = group.sortWith((a, b) => a.leafIndex < b.leafIndex)
      "            ['Position" + sorted.head.section + "'," + sorted.map(x => x.value).map(d => fmt(d)).mkString(", ") + "]"
    }

    def leavesToString(leaves: Seq[Int]): String = {
      "            ['Leaf'," + leaves.mkString(", ") + "]"
    }

    val transData = LeafTransmission.getByOutput(outputPK).map(v => LeafValue(v.section, v.leafIndex, v.transmission_fract))
    val transLeaves = transData.map(_.section).distinct.sorted
     */
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
      val url = ViewOutput.path + "?outputPK=" + output.outputPK.get + "&summary=true"
      <a href={url}>Files</a>
    }

    def spreadSheetLinks(fileWorkbookList: Seq[LOCFileWorkbook]): IndexedSeq[Elem] = {
      val spaces = nbsp + " " + nbsp + " " + nbsp + " " + nbsp + " " + nbsp

      def links(fs: LOCFileWorkbook) = {
        val base = Util.fileBaseName(fs.file)
        Seq(
          { <div><a href={base + ".html"} title="View HTML version of spreadsheet">View {base}</a>{spaces}</div> },
          { <div><a href={fs.file.getName} title="Download spreadsheet">Download {base}</a>{spaces}</div> }
        )
      }

      if (fileWorkbookList.isEmpty) {
        IndexedSeq({ <div title="There were not spreadsheets found.  Check the log for possible errors.">No Spreadsheets</div> })
      } else fileWorkbookList.flatMap(fw => links(fw)).toIndexedSeq
    }

    val viewLog: Elem = {
      <a href={StdLogger.LOG_TEXT_FILE_NAME}>View Log</a>
    }

    val viewXml: Elem = {
      <a href={ProcedureOutputUtil.outputFileName}>View XML</a>
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
        <div class={"col-md-" + col}>{elem}</div>
      }

      def wrap2(col: Int, name: String, value: String): Elem = {
        <div class={"col-md-" + col}><em>{name}:</em><br/>{value}</div>
      }

      def wrap2Anon(col: Int, name: String, value: String): Elem = {
        <div class={"col-md-" + col}><em>{name}:</em><br/><span aqaalias="">{value}</span></div>
      }

      val redoLink = {
        <a href={"/view/OutputList?redo=" + output.outputPK.get}>Redo</a>
      }

      val div = {
        <div class="row col-md-10 col-md-offset-1">
          <div class="row">
            <div class="col-md-1" title="Leaf Offset Constancy and Transmission"><h2>LOC</h2></div>
            <div class="col-md-2 col-md-offset-1" title="Machine"><h2 aqaalias="">{machineId}</h2></div>
            <div class="col-md-3 col-md-offset-1">EPID Center Correction in mm: {epidCenterCorrection}</div>
          </div>
          <div class="row" style="margin:20px;">
            {wrap2Anon(1, "Institution", institutionName)}
            {wrap2(2, "Data Acquisition", dateToString(output.dataDate))}
            {wrap2(2, "Analysis", analysisDate)}
            {wrap2Anon(1, "Analysis by", userId)}
            {wrap2Anon(1, "Elapsed", elapsed)}
            {wrap2(2, "Procedure", procedureDesc)}
            {wrap(1, redoLink)}
          </div>
          <div class="row" style="margin:20px;">
            {spreadSheetLinks(fileWorkbookList).map(e => { wrap(3, e) })}
          </div>
          <div class="row" style="margin:20px;">
            {wrap(2, linkToFiles)}
            {wrap(2, viewLog)}
            {wrap(2, viewXml)}
          </div>
          <div class="row">
            {
          if (numberOfBadValues > 0) {
            <div style="color:red">
                <h3>Caution: There were {numberOfBadValues} invalid values in the results</h3>
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

      wrapBody(div, "LOC", None, c3 = true, Some(runScript))
    }
    logger.info("Making LOC display for output " + output.outputPK.get)
    make
  }
}
