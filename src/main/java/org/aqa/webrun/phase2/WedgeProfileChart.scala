package org.aqa.webrun.phase2

import org.aqa.Logging
import org.aqa.db.CenterDose
import java.text.SimpleDateFormat
import org.aqa.Util
import org.aqa.db.Wedge

object WedgeProfileChart extends Logging {

  def beamRef(beamName: String): String = "ChartProfile_" + beamName.replaceAll("[^a-zA-Z0-9]", "_")

  def chart(wedgeListUnsorted: Seq[Wedge]) = {

    // make sure they are spatially sorted
    val wedgeList = wedgeListUnsorted.sortWith((a, b) => a.position_mm < b.position_mm)
    val beamName = wedgeList.head.beamName

    val posnList = wedgeList.map(w => w.position_mm).mkString("[ 'Position mm', ", ", ", " ]")
    val huList = wedgeList.map(w => w.radiodensity_hu).mkString("[ 'HU', ", ", ", " ]")

    val tag = "@@BR@@"

    val template = """

        var @@BR@@ = c3.generate({
                data: {
                    x: 'Position mm',
                    columns: [
                         """ + posnList + """,
                         """ + huList + """
                    ]
                },
                point: { // show point on hover
                    r: 0,
                    focus : {
                        expand: {
                            r:4
                        }
                    }
                },
                bindto : '#@@BR@@',
                axis: {
                    x: {
                        label: 'Position mm',
                        tick: {
                            format: d3.format('.3g')
                        }
                    },
                    y: {
                        label: 'HU',
                        tick: {
                            format: d3.format('.4g')
                        }
                    }
                },
                color : {
                    pattern : [ '#6688bb' ]
                }
            });
"""

    template.replace(tag, beamRef(beamName))

  }

}
