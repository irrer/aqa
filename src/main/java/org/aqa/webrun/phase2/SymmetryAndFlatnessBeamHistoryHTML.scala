package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.SymmetryAndFlatness
import scala.xml.Elem
import org.aqa.web.WebUtil
import java.sql.Timestamp
import org.aqa.Util

/**
 * Analyze DICOM files for symmetry and flatness.
 */
class SymmetryAndFlatnessBeamHistoryHTML(beamName: String, extendedData: ExtendedData) extends Logging {

  private val history = SymmetryAndFlatness.recentHistory(50, extendedData.machine.machinePK.get, extendedData.output.procedurePK, beamName, extendedData.output.dataDate)
  private val dateList = history.map(h => h.date)
  private val dateListFormatted = dateList.map(d => Util.standardDateFormat.format(d))

  private def idOf(typ: String): String = "Chart_" + WebUtil.stringToUrlSafe(beamName) + "_" + typ

  private val tagAxial = "AxialSymmetry"
  private val tagTransverse = "TransverseSymmetry"
  private val tagFlatness = "FlatnessSymmetry"

  private val idAxial = idOf(tagAxial)
  private val idTransverse = idOf(tagTransverse)
  private val idFlatness = idOf(tagFlatness)

  val graphAxial = graph(idAxial, history.map(h => h.symmetryAndFlatness.axialSymmetry_mm))
  val graphTransverse = graph(idTransverse, history.map(h => h.symmetryAndFlatness.transverseSymmetry_mm))
  val graphFlatness = graph(idFlatness, history.map(h => h.symmetryAndFlatness.flatness_mm))

  val graphAll = graphAxial + graphTransverse + graphFlatness

  val htmlAxial = html(idAxial)
  val htmlTransverse = html(idAxial)
  val htmlFlatness = html(idAxial)

  private def html(id: String): Elem = { <div id={ id }>filler</div> }

  private def graph(id: String, valueList: Seq[Double]): String = {

    val currentDateIndex = dateList.indexWhere(d => extendedData.output.dataDate.get.getTime == d.getTime)
    val minDateTag = dateListFormatted.head
    val maxDateTag = dateListFormatted.last
    def javascript = {
      """

        var """ + id + """ = c3.generate({
                data: {
                    x: 'Date',
                    xFormat: '%Y-%m-%dT%H:%M:%S',
                    columns: [
                         [ 'Date', """ + dateListFormatted.mkString(", ") + """],
                         [ 'Value', """ + valueList.mkString(", ") + """]
                    ],
                    color: function (color, d) {
                        return (d.index == """ + currentDateIndex.toString + """) ? 'orange' : color ;
                    }
                },
                bindto : '#""" + id + """',
                axis: {
                    x: {
                        label: 'Date',
                        type: 'timeseries',
                        min: '""" + minDateTag + """',
                        max: '""" + maxDateTag + """',
                        tick: { format: function(dt) { return formatDate(dt); } }
                    },
                    y: {
                        label: 'Value',
                        tick: {
                            format: d3.format('.4f')
                        }
                    }
                },
                color : {
                    pattern : [ '#6688bb' ]
                },
                padding: {
                  right: 30,
                  top: 10
                }
            });
"""
    }

    javascript
  }

}
