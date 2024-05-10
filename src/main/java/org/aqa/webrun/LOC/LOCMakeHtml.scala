package org.aqa.webrun.LOC

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.EPIDCenterCorrection
import org.aqa.db.Output
import org.aqa.run.StdLogger
import org.aqa.web.OutputHeading
import org.aqa.web.ViewOutput
import org.aqa.web.WebUtil._

import scala.xml.Elem

object LOCMakeHtml extends Logging {

  private def okDbl(d: Double) = { (d < Double.MaxValue) && (d > Double.MinValue) }

  def makeDisplay(output: Output, locXml: LOCToXml, fileWorkbookList: Seq[LOCFileWorkbook]): String = {

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

    val epidCenterCorrection: String = {
      EPIDCenterCorrection.getByOutput(output.outputPK.get).headOption match {
        case Some(ecc) => ecc.epidCenterCorrection_mm.formatted("%6.3f")
        case _         => "not available"
      }
    }

    def fmt(d: Double): String = d.formatted("%7.5e")

    val leavesText = locXml.leafIndexList.distinct.sorted.mkString("            ['Leaf', ", ", ", "]")

    def twoD2Text(data: Seq[Seq[Double]]): String = {
      def sec2String(s: Int): String = {
        val textNumbers = data.map(v => v(s)).map(d => fmt(d))
        textNumbers.mkString("            ['Position" + (s + 1) + "', ", ", ", "]")
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
      <a href={LOCUtil.locXmlOutputFileName}>View XML</a>
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

      val div = {
        <div class="row col-md-10 col-md-offset-1">
          <div class="row">
            <div class="col-md-3 col-md-offset-1">EPID Center Correction in mm: {epidCenterCorrection}</div>
          </div>
          <div class="row" style="margin:20px;">
            {OutputHeading.reference(output.outputPK.get)}
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
