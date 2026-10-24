package org.aqa.webrun.psm

import org.aqa.db.FloodField
import org.aqa.db.Output
import org.aqa.db.PSMBeam
import org.aqa.db.SymmetryAndFlatness
import org.aqa.Config
import org.aqa.Logging

import java.awt.geom.Point2D
import java.security.InvalidParameterException
import java.util.Date

/**
 * Cache PSM and flood field data.
 */
object PSMCache extends Logging {

  /**
   * Values stored in this cache.
   */
  case class PSMGridWithFF(psmGrid: PSMGrid, floodField: FloodField) {

    private def meanOfNormalizedFloodField_cu(xCenter_mm: Double, yCenter_mm: Double): Double = {

      val floodFieldPixelCoordinates = PSMUtil.pixelCoordinatesWithinRadius( //
        floodField.trans,
        floodField.dicomImageNormalized,
        floodField.trans.iso2Pix(new Point2D.Double(xCenter_mm, yCenter_mm))
      )

      val pixelValueList = floodFieldPixelCoordinates.map(c => floodField.dicomImageNormalized.get(c.getX, c.getY))
      pixelValueList.sum.toDouble / pixelValueList.size

    }

    /** Mean CU of top area of normalized flood beam. */
    val floodTop_cu: Double = meanOfNormalizedFloodField_cu(psmGrid.topBeam.xCenter_mm, psmGrid.topBeam.yCenter_mm)

    /** Mean CU of bottom area of normalized flood beam. */
    val floodBottom_cu: Double = meanOfNormalizedFloodField_cu(psmGrid.bottomBeam.xCenter_mm, psmGrid.bottomBeam.yCenter_mm)

    /** Mean CU of left area of normalized flood beam. */
    val floodLeft_cu: Double = meanOfNormalizedFloodField_cu(psmGrid.leftBeam.xCenter_mm, psmGrid.leftBeam.yCenter_mm)

    /** Mean CU of right area of normalized flood beam. */
    val floodRight_cu: Double = meanOfNormalizedFloodField_cu(psmGrid.rightBeam.xCenter_mm, psmGrid.rightBeam.yCenter_mm)

    /** Mean CU of center area of normalized flood beam. */
    val floodCenter_cu: Double = meanOfNormalizedFloodField_cu(psmGrid.centerBeam.xCenter_mm, psmGrid.centerBeam.yCenter_mm)


    def ffTimesPSMTop(): Double = {
      psmGrid.topBeam.mean_cu * psmGrid.topBeam.getFloodField_cu(this)
    }

    def ffTimesPSMBottom(): Double = {
      psmGrid.bottomBeam.mean_cu * psmGrid.bottomBeam.getFloodField_cu(this)
    }

    def ffTimesPSMLeft(): Double = {
      psmGrid.leftBeam.mean_cu * psmGrid.leftBeam.getFloodField_cu(this)
    }

    def ffTimesPSMRight(): Double = {
      psmGrid.rightBeam.mean_cu * psmGrid.rightBeam.getFloodField_cu(this)
    }

    def ffTimesPSMCenter(): Double = {
      psmGrid.centerBeam.mean_cu * psmGrid.centerBeam.getFloodField_cu(this)
    }

    // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

    // @formatter:off
    def normalizedTop   : Double = ffTimesPSMTop   () / ffTimesPSMCenter()
    def normalizedBottom: Double = ffTimesPSMBottom() / ffTimesPSMCenter()
    def normalizedLeft  : Double = ffTimesPSMLeft  () / ffTimesPSMCenter()
    def normalizedRight : Double = ffTimesPSMRight () / ffTimesPSMCenter()
    def normalizedCenter: Double = ffTimesPSMCenter() / ffTimesPSMCenter()
    // @formatter:on

  }

  /**
   * Internal cache representation.
   */
  private case class Mach(machinePK: Long, psmGridList: Seq[PSMGrid], floodFieldList: Seq[FloodField]) {

    def findMatch(dataDate: Date, KVP: Double, isFFF: Boolean, span_mm: Double, diameter_mm: Double): Option[PSMGridWithFF] = {

      // the latest matching flood field captured before the given date
      val floodField = {
        def qualifies(floodField: FloodField): Boolean = {
          (floodField.dataDate_ms < dataDate.getTime) &&
            (floodField.KVP == KVP) &&
            (floodField.FlatteningFilterFree == isFFF)
        }

        floodFieldList.filter(qualifies).sortBy(_.dataDate_ms).lastOption
      }

      def spansAreApproximatelyEqual(psmGridSpan_mm: Double): Boolean = {
        (psmGridSpan_mm - span_mm).abs < SymmetryAndFlatness.SpanTolerance_mm
      }

      def diametersAreApproximatelyEqual(): Boolean = {
        ((Config.PSMRadius_mm * 2) - diameter_mm).abs < SymmetryAndFlatness.DiameterTolerance_mm
      }


      // find a PSM that was captured after the flood field but before the given date.
      val psm = {
        if (floodField.isEmpty)
          None
        else {
          // True if PSM was captured after the flood field but before the given date.
          def qualifies(psmGrid: PSMGrid): Boolean = {
            (psmGrid.dataDate_ms < dataDate.getTime) &&
              (psmGrid.dataDate_ms > floodField.get.dataDate_ms) &&
              spansAreApproximatelyEqual(psmGrid.span) &&
              diametersAreApproximatelyEqual()

          }

          psmGridList.filter(qualifies).sortBy(_.dataDate_ms).lastOption
        }
      }

      if (psm.isEmpty)
        None
      else
        Some(PSMGridWithFF(psm.get, floodField.get))

    }

  }

  /** Keep copy of current PSM data here. */
  private val psmAndFloodFieldCache = scala.collection.mutable.HashMap[Long, Mach]()

  /** Keep copy of current outputs here. */
  private val outputForPSMCache = scala.collection.mutable.HashMap[Long, Option[Output]]()

  /**
   * Get all the relevant data for a given machine and put it in the cache.
   *
   * @param machinePK For this machine.
   */
  private def populate(machinePK: Long): Unit = {
    val psmBeamList = PSMBeam.getByMachine(machinePK)
    val gridList = psmBeamList.groupBy(_.outputPK).values.map(PSMGrid.makePSMGrid).toSeq

    val ffList = FloodField.getByMachine(machinePK)

    val mach = Mach(machinePK = machinePK, psmGridList = gridList, floodFieldList = ffList)

    psmAndFloodFieldCache.synchronized {
      psmAndFloodFieldCache.put(machinePK, mach)
    }
  }

  private def populateOutput(outputPK: Long): Unit = {
    val output = Output.get(outputPK)

    outputForPSMCache.synchronized {
      outputForPSMCache.put(outputPK, output)
    }
  }

  /**
   * Remove the records for this machine, if there are any.
   *
   * If there is no record for this machine then noting is changed.
   *
   * @param machinePK For this machine.
   */
  def clearMachine(machinePK: Long): Unit = {
    psmAndFloodFieldCache.synchronized {
      psmAndFloodFieldCache.remove(machinePK)
    }
    logger.info(s"Cleared PSM and flood field data for machine $machinePK")
  }

  /**
   * Get the PSM and flood field data for the given parameters.
   *
   * @param machinePK This machine.
   * @param dataDate  Before this date.
   * @param KVP       This energy.
   * @param isFFF     True if FFF required, otherwise a standard beam.
   * @return PSM info if available, otherwise None.
   */
  def get(machinePK: Long, dataDate: Date, KVP: Double, isFFF: Boolean, span_mm: Double, diameter_mm: Double): Option[PSMGridWithFF] = {

    if (!psmAndFloodFieldCache.contains(machinePK))
      populate(machinePK)

    psmAndFloodFieldCache(machinePK).findMatch(dataDate, KVP, isFFF, span_mm, diameter_mm)
  }

  /**
   * Get the PSM and flood field data for the given parameters.
   *
   * @param outputPK Specifies both machine and data date.
   * @param KVP      This energy.
   * @param isFFF    True if FFF required, otherwise a standard beam.
   * @return PSM info if available, otherwise None.
   */
  def get(outputPK: Long, KVP: Double, isFFF: Boolean, span_mm: Double, diameter_mm: Double): Option[PSMGridWithFF] = {

    if (!outputForPSMCache.contains(outputPK))
      populateOutput(outputPK)

    val output = outputForPSMCache(outputPK)

    if (output.isEmpty || output.get.machinePK.isEmpty || output.get.dataDate.isEmpty)
      throw new InvalidParameterException(s"Unexpected exception.  outputPK must exist and machinePK and dataDate must be non-empty: $outputPK")

    get(output.get.machinePK.get, output.get.dataDate.get, KVP, isFFF, span_mm, diameter_mm)
  }

  /**
   * Get the PSM and flood field data for the given parameters.
   *
   * @param sf For this SymmetryAndFlatness item.
   * @return PSM info if available, otherwise None.
   */
  def get(sf: SymmetryAndFlatness): Option[PSMGridWithFF] = {
    if ( //
      sf.KVP.isDefined &&
        sf.isFlatteningFilterFree.isDefined &&
        sf.span_mm.isDefined &&
        sf.diameter_mm.isDefined
    )
      get( //
        outputPK = sf.outputPK,
        KVP = sf.KVP.get,
        isFFF = sf.isFlatteningFilterFree.get,
        span_mm = sf.span_mm.get,
        diameter_mm = sf.diameter_mm.get
      )
    else
      None
  }

}
