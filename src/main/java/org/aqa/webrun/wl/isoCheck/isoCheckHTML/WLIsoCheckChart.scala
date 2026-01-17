package org.aqa.webrun.wl.isoCheck.isoCheckHTML

import org.aqa.db.IsoCheck
import org.aqa.db.IsoCheck.IsoCheckHistory
import org.aqa.db.MaintenanceRecord
import org.aqa.db.Output
import org.aqa.db.WinstonLutz
import org.aqa.db.WinstonLutzGeneric
import org.aqa.web.C3ChartHistory
import org.aqa.webrun.wl.isoCheck.WLIsoTable

import java.awt.Color

class WLIsoCheckChart(outputPK: Long) {

  private val output = Output.get(outputPK).get

  private val machinePK = output.machinePK.get

  private val history = IsoCheck.history(machinePK)

  private val xDateList = history.map(h => h.output.dataDate.get)

  private def hasSevenBeamsData(h: IsoCheckHistory): Boolean = {
    h.isoTable.isDefined &&
    h.isoTable.get.T__0.isDefined &&
    h.isoTable.get.T_30.isDefined &&
    h.isoTable.get.T_60.isDefined &&
    h.isoTable.get.T_90.isDefined &&
    h.isoTable.get.T270.isDefined &&
    h.isoTable.get.T300.isDefined &&
    h.isoTable.get.T330.isDefined
  }

  // ------------------------------------------------------------------------------------------------

  private def makeIsoCheckChart(): C3ChartHistory = {

    // list of all MaintenanceRecords in this time interval
    val MaintenanceRecordList = {
      val first = history.head.output.dataDate.get
      val last = history.last.output.dataDate.get
      MaintenanceRecord.getRange(machinePK, first, last)
    }

    val yAxisLabels = Seq(
      "CBCT - Gantry Iso X",
      "CBCT - Gantry Iso Y",
      "CBCT - Gantry Iso Z",
      "Gantry Flex",
      "Col-Gantry misalignment",
      "MLC offset",
      "Gantry Isocentricity",
      "Collimator Isocentricity",
      "Maximum R",
      "Maximum R Table 0"
    )

    val yValues: Seq[Seq[Double]] = {
      Seq(
        history.map(-_.isoCheck.isoX),
        history.map(-_.isoCheck.isoY),
        history.map(-_.isoCheck.isoZ),
        history.map(_.isoCheck.gantryFlex),
        history.map(_.isoCheck.collGantryMisalign),
        history.map(_.isoCheck.mlcOffsetY),
        history.map(_.isoCheck.gantryIsocentricity),
        history.map(_.collimator.get_CA_Rpp_Optimized),
        history.map(_.maxR),
        history.map(_.isoCheck.maxR)
      )
    }

    val yIndex = history.indexWhere(_.output.outputPK.get == outputPK)

    val colorList = Seq(
      new Color(104, 187, 154),
      new Color(104, 187, 112),
      new Color(137, 187, 104),
      new Color(102, 136, 187),
      new Color(50, 50, 50),
      new Color(100, 100, 100),
      new Color(248, 0, 0),
      new Color(158, 0, 0),
      new Color(100, 100, 255),
      new Color(50, 50, 255)
    )

    val isoCheckChart = new C3ChartHistory(
      chartIdOpt = Some("IsoCheck"),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yAxisLabels,
      yDataLabel = "mm",
      yValues = yValues,
      yIndex = yIndex,
      yFormat = ".4g",
      yColorList = colorList,
      setBaselineList = Seq()
    )

    isoCheckChart

  }

  // ------------------------------------------------------------------------------------------------

  private def makeIsoTableChart(): C3ChartHistory = {
    val isoTableList = history.filter(h => h.isoTable.isDefined)

    // list of all MaintenanceRecords in this time interval
    val MaintenanceRecordList = {
      val first = isoTableList.head.output.dataDate.get
      val last = isoTableList.last.output.dataDate.get
      MaintenanceRecord.getRange(machinePK, first, last)
    }

    val yAxisLabels = Seq(
      "Table - Gantry Iso X",
      "Table - Gantry Iso Z",
      "Table Isocentricity"
    )

    val yValues: Seq[Seq[Double]] = {
      Seq(
        isoTableList.map(h => h.isoTable.get.get_IsoTable_X_Optimized - h.isoCheck.isoX),
        isoTableList.map(h => h.isoTable.get.get_IsoTable_Z_Optimized - h.isoCheck.isoZ),
        isoTableList.map(h => Math.sqrt(h.isoTable.get.get_RSquared_Optimized))
      )
    }

    val yIndex = isoTableList.indexWhere(_.output.outputPK.get == outputPK)

    val colorList = Seq(
      new Color(104, 187, 154),
      new Color(104, 187, 112),
      new Color(137, 187, 104)
    )

    val isoTableChart = new C3ChartHistory(
      chartIdOpt = Some("IsoTable"),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yAxisLabels,
      yDataLabel = "mm",
      yValues = yValues,
      yIndex = yIndex,
      yFormat = ".4g",
      yColorList = colorList,
      setBaselineList = Seq()
    )

    isoTableChart
  }

  // ------------------------------------------------------------------------------------------------

  private def makeIsoTableBB_RSqChartThreeBeams(): C3ChartHistory = {

    val isoTableList = history.filter(h => h.isoTable.isDefined)

    // list of all MaintenanceRecords in this time interval
    val MaintenanceRecordList = {
      val first = isoTableList.head.output.dataDate.get
      val last = isoTableList.last.output.dataDate.get
      MaintenanceRecord.getRange(machinePK, first, last)
    }

    val yAxisLabels = Seq(
      "T0",
      "T90",
      "T270"
    )

    def BB_RppSq(it: WLIsoTable, beam: WinstonLutzGeneric): Double = {
      it.BB_Rpp(beam, it.get_dXT__0_Optimized, it.get_dZT__0_Optimized, it.get_IsoTable_X_Optimized, it.get_IsoTable_Z_Optimized)
    }

    val yValues: Seq[Seq[Double]] = {
      Seq(
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T__0.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T_90.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T270.get))
      )
    }

    val yIndex = isoTableList.indexWhere(_.output.outputPK.get == outputPK)

    val colorList = Seq(
      new Color(104, 187, 154), //  0
      new Color(102, 136, 187), // 90
      new Color(50, 50, 50) //   270
    )

    val isoTableBB_RSqChart = new C3ChartHistory(
      chartIdOpt = Some("IsoTableBB_RppSqThreeBeams"),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yAxisLabels,
      yDataLabel = "mm",
      yValues = yValues,
      yIndex = yIndex,
      yFormat = ".4g",
      yColorList = colorList,
      setBaselineList = Seq()
    )

    isoTableBB_RSqChart
  }

  // ------------------------------------------------------------------------------------------------

  private def makeIsoTableBB_RSqChartSevenBeams(): C3ChartHistory = {

    val isoTableList = history.filter(hasSevenBeamsData)

    // list of all MaintenanceRecords in this time interval
    val MaintenanceRecordList = {
      val first = isoTableList.head.output.dataDate.get
      val last = isoTableList.last.output.dataDate.get
      MaintenanceRecord.getRange(machinePK, first, last)
    }

    val yAxisLabels = Seq(
      "T0",
      "T30",
      "T60",
      "T90",
      "T270",
      "T300",
      "T330"
    )

    def BB_RppSq(it: WLIsoTable, beam: WinstonLutzGeneric): Double = {
      it.BB_Rpp(beam, it.get_dXT__0_Optimized, it.get_dZT__0_Optimized, it.get_IsoTable_X_Optimized, it.get_IsoTable_Z_Optimized)
    }

    val yValues: Seq[Seq[Double]] = {
      Seq(
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T__0.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T_30.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T_60.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T_90.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T270.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T300.get)),
        isoTableList.map(h => BB_RppSq(h.isoTable.get, h.isoTable.get.T330.get))
      )
    }

    val yIndex = isoTableList.indexWhere(_.output.outputPK.get == outputPK)

    val colorList = Seq(
      new Color(104, 187, 154), //   0
      new Color(104, 187, 112), //  30
      new Color(137, 187, 104), //  60
      new Color(102, 136, 187), //  90
      new Color(50, 50, 50), //    270
      new Color(100, 100, 100), // 300
      new Color(100, 200, 200) //  330
    )

    val isoTableBB_RSqChart = new C3ChartHistory(
      chartIdOpt = Some("IsoTableBB_RppSqSevenBeams"),
      MaintenanceRecordList,
      width = None,
      height = None,
      xLabel = "Date",
      Seq(xDateList),
      baseline = None,
      tolerance = None,
      yRange = None,
      yAxisLabels = yAxisLabels,
      yDataLabel = "mm",
      yValues = yValues,
      yIndex = yIndex,
      yFormat = ".4g",
      yColorList = colorList,
      setBaselineList = Seq()
    )

    isoTableBB_RSqChart
  }

  // ------------------------------------------------------------------------------------------------

  val isoCheckChart: C3ChartHistory = makeIsoCheckChart()
  val isoTableChart: Option[C3ChartHistory] = if (history.exists(_.isoTable.isDefined)) Some(makeIsoTableChart()) else None
  val isoTableBB_RSqChartThreeBeams: Option[C3ChartHistory] = if (history.exists(_.isoTable.isDefined)) Some(makeIsoTableBB_RSqChartThreeBeams()) else None
  val isoTableBB_RSqChartSevenBeams: Option[C3ChartHistory] = if (history.exists(hasSevenBeamsData)) Some(makeIsoTableBB_RSqChartSevenBeams()) else None

}
