package org.aqa.webrun.focalSpot

import edu.umro.DicomDict.TagByName
import org.aqa.Config

object FSMatlab {

  private def NominalBeamEnergyText(fsMeasure: FSMeasure): String = {
    if (fsMeasure.NominalBeamEnergy == fsMeasure.NominalBeamEnergy.round) fsMeasure.NominalBeamEnergy.round.toString else fsMeasure.NominalBeamEnergy.toString
  }

  // format all numbers like this
  private val f = "%20.16f"

  def focalSpotEdgeMatlabText(fsMeasure: FSMeasure): String = {

    val mvText = NominalBeamEnergyText(fsMeasure)

    val typeOf = if (fsMeasure.isMLC) "MLC" else "Jaw"
    val prefix = typeOf + "_" + fsMeasure.collimatorAngleRounded_deg.formatted("%03d") + "_"

    val translation = fsMeasure.rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues.toSeq
    val RTImageSID = fsMeasure.rtimage.get(TagByName.RTImageSID).getDoubleValues.head
    val RadiationMachineSAD = fsMeasure.rtimage.get(TagByName.RadiationMachineSAD).getDoubleValues.head
    val ImagePlanePixelSpacing = fsMeasure.rtimage.get(TagByName.ImagePlanePixelSpacing).getDoubleValues.toSeq

    val top_px = fsMeasure.analysisResult.measurementSet.top
    val bottom_px = fsMeasure.analysisResult.measurementSet.bottom
    val left_px = fsMeasure.analysisResult.measurementSet.left
    val right_px = fsMeasure.analysisResult.measurementSet.right

    /* Construct the Matlab text for the image position of the upper right corner of the image. */
    val RTImagePositionText = {
      val RTImagePositionAttr = fsMeasure.rtimage.get(TagByName.RTImagePosition)

      val calcText = {
        if (RTImagePositionAttr == null) {
          // The RTImagePosition attribute is not available.
          val Columns = fsMeasure.rtimage.get(TagByName.Columns).getIntegerValues.head
          val Rows = fsMeasure.rtimage.get(TagByName.Rows).getIntegerValues.head
          s"""
             |
             |% This attribute is missing from the RTIMAGE file: DICOM 3002,0012 RTImagePosition : RT Image Plane, Position and Orientation (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020012
             |% So instead it is being calculated using the pixel sizes (in mm) and size (in pixels) of the imager.
             |
             |% DICOM 0028,0010 Rows : Number of rows of pixels in the image : https://dicom.innolitics.com/ciods/rt-image/image-pixel/00280010
             |${prefix}Rows = $Rows;
             |
             |% DICOM 0028,0011 Columns : Number of columns of pixels in the image : https://dicom.innolitics.com/ciods/rt-image/image-pixel/00280011
             |${prefix}Columns = $Columns;
             |
             |${prefix}imagePosX = -(((${prefix}Columns - 1.0) / 2) * ${prefix}pixSizeX) / ${prefix}beamExpansion;
             |${prefix}imagePosY =  (((${prefix}Rows    - 1.0) / 2) * ${prefix}pixSizeY) / ${prefix}beamExpansion;
             |
             |""".stripMargin
        } else {
          val RTImagePosition = RTImagePositionAttr.getDoubleValues
          s"""
             |
             |% DICOM 3002,0012 RTImagePosition : RT Image Plane, Position and Orientation (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020012
             |${prefix}imagePosX = ${RTImagePosition.head} / ${prefix}beamExpansion;
             |${prefix}imagePosY = ${RTImagePosition(1)} / ${prefix}beamExpansion;
             |fprintf("\\n");
             |
             |""".stripMargin
        }
      }

      // print the results for imagePosX and imagePosY
      val printText = {
        s"""
           |
           |fprintf("${prefix}imagePosX: $f\\n", ${prefix}imagePosX);
           |fprintf("${prefix}imagePosY: $f\\n", ${prefix}imagePosY);
           |fprintf("\\n");
           |
           |""".stripMargin
      }
      calcText + printText
    }

    val measureText: String = {
      s"""
         |% -------------------------------------------------------------------------------------------------------------------
         |
         |% -------------------------------------------- MV $mvText --------------------------------------------
         |
         |fprintf("\\n\\n----------- MV $mvText ------------------\\n\\n");
         |
         |fprintf("Matlab code calculating edge positions used for focal spot calculation for $typeOf ${fsMeasure.collimatorAngleRounded_deg} for MV $mvText\\n");
         |
         |% DICOM 3002,000D XRayImageReceptorTranslation : https://dicom.innolitics.com/ciods/rt-image/rt-image/3002000d
         |${prefix}transX = ${translation.head};
         |${prefix}transY = ${translation(1)};
         |${prefix}transZ = ${translation(2)};
         |fprintf("${prefix}transX: $f\\n", ${prefix}transX);
         |fprintf("${prefix}transY: $f\\n", ${prefix}transY);
         |fprintf("${prefix}transZ: $f\\n", ${prefix}transZ);
         |
         |% DICOM 3002,0026 RTImageSID : Distance from radiation machine source to image plane (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020026
         |${prefix}SID = $RTImageSID;
         |fprintf("${prefix}SID: $f\\n", ${prefix}SID);
         |
         |% DICOM 3002,0022 RadiationMachineSAD  : Radiation source to Gantry rotation axis distance (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020022
         |${prefix}SAD = $RadiationMachineSAD;
         |fprintf("${prefix}SAD: $f\\n", ${prefix}SAD);
         |
         |% DICOM 3002,0011 ImagePlanePixelSpacing : Pixel size X and Y (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020011
         |${prefix}pixSizeX = ${ImagePlanePixelSpacing.head};
         |${prefix}pixSizeY = ${ImagePlanePixelSpacing(1)};
         |fprintf("${prefix}pixSizeX: $f\\n", ${prefix}pixSizeX);
         |fprintf("${prefix}pixSizeY: $f\\n", ${prefix}pixSizeY);
         |
         |% Expansion ratio between isoplane and image plane
         |${prefix}beamExpansion = ${prefix}SID / ${prefix}SAD;
         |fprintf("${prefix}beamExpansion: $f\\n", ${prefix}beamExpansion);
         |fprintf("\\n");
         |
         |$RTImagePositionText
         |
         |% Expansion ratio between isoplane and image plane for X and Y pixels
         |${prefix}expandX = ${prefix}beamExpansion / ${prefix}pixSizeX;
         |${prefix}expandY = ${prefix}beamExpansion / ${prefix}pixSizeY;
         |fprintf("${prefix}expandX: $f\\n", ${prefix}expandX);
         |fprintf("${prefix}expandY: $f\\n", ${prefix}expandY);
         |fprintf("\\n");
         |
         |% Edge measurements in pixel coordinates for $prefix
         |${prefix}top_px    = $top_px;
         |${prefix}bottom_px = $bottom_px;
         |${prefix}left_px   = $left_px;
         |${prefix}right_px  = $right_px;
         |fprintf("${prefix}top_px    : $f\\n", ${prefix}top_px);
         |fprintf("${prefix}bottom_px : $f\\n", ${prefix}bottom_px);
         |fprintf("${prefix}left_px   : $f\\n", ${prefix}left_px);
         |fprintf("${prefix}right_px  : $f\\n", ${prefix}right_px);
         |fprintf("\\n");
         |
         |% Edge measurements in mm in isoplane for $prefix
         |${prefix}top_mm    = ( ${prefix}top_px    / ${prefix}expandY ) - ${prefix}imagePosY - ${prefix}transY;
         |${prefix}bottom_mm = ( ${prefix}bottom_px / ${prefix}expandY ) - ${prefix}imagePosY - ${prefix}transY;
         |${prefix}left_mm   = ( ${prefix}left_px   / ${prefix}expandX ) + ${prefix}imagePosX + ${prefix}transX;
         |${prefix}right_mm  = ( ${prefix}right_px  / ${prefix}expandX ) + ${prefix}imagePosX + ${prefix}transX;
         |fprintf("${prefix}top_mm    : $f\\n", ${prefix}top_mm);
         |fprintf("${prefix}bottom_mm : $f\\n", ${prefix}bottom_mm);
         |fprintf("${prefix}left_mm   : $f\\n", ${prefix}left_mm);
         |fprintf("${prefix}right_mm  : $f\\n", ${prefix}right_mm);
         |fprintf("\\n");
         |
         |fprintf("Edge results calculated by AQA that were stored in the database (for verification).\\n");
         |fprintf("$prefix top    AQA_mm : $f\\n", ${fsMeasure.focalSpot.topEdge_mm});
         |fprintf("$prefix bottom AQA_mm : $f\\n", ${fsMeasure.focalSpot.bottomEdge_mm});
         |fprintf("$prefix left   AQA_mm : $f\\n", ${fsMeasure.focalSpot.leftEdge_mm});
         |fprintf("$prefix right  AQA_mm : $f\\n", ${fsMeasure.focalSpot.rightEdge_mm});
         |fprintf("\\n");
         |
         |fprintf("\\n\\n----------- MV $mvText ------------------\\n\\n");
         |
         |% -------------------------------------------- MV $mvText --------------------------------------------
         |
         |% -------------------------------------------------------------------------------------------------------------------
         |""".stripMargin
    }

    measureText
  }

  def focalSpotAggregateMatlabText(fsSet: FSSet): String = {

    val mvText = NominalBeamEnergyText(fsSet.jaw090)

    // calculate the average epid to source value
    val dEpid_mm: Double = (fsSet.jaw090.dEpid_mm + fsSet.jaw270.dEpid_mm + fsSet.mlc090.dEpid_mm + fsSet.mlc270.dEpid_mm) / 4.0

    val focalSpotText = {
      s"""
         |% -------------------------------------------- Focal Spot Measurement for MV $mvText --------------------------------------------
         |
         |fprintf("Jaw 090 top bottom right left:  $f    $f    $f    $f\\n", Jaw_090_top_mm, Jaw_090_bottom_mm, Jaw_090_left_mm, Jaw_090_right_mm);
         |fprintf("Jaw 270 top bottom right left:  $f    $f    $f    $f\\n", Jaw_270_top_mm, Jaw_270_bottom_mm, Jaw_270_left_mm, Jaw_270_right_mm);
         |fprintf("MLC 090 top bottom right left:  $f    $f    $f    $f\\n", MLC_090_top_mm, MLC_090_bottom_mm, MLC_090_left_mm, MLC_090_right_mm);
         |fprintf("MLC 270 top bottom right left:  $f    $f    $f    $f\\n", MLC_270_top_mm, MLC_270_bottom_mm, MLC_270_left_mm, MLC_270_right_mm);
         |
         |% distance in mm from radiation source to X jaw, Y jaw, and collimator.
         |xJaw = ${Config.TrueBeamSourceToXJawDistance_mm};
         |yJaw = ${Config.TrueBeamSourceToYJawDistance_mm};
         |MLC = ${Config.TrueBeamSourceToMLCDistance_mm};
         |
         |fprintf("Source to X jaws mm: $f\\n", JawX);
         |fprintf("Source to Y jaws mm: $f\\n", JawY);
         |fprintf("Source to MLC    mm: $f\\n", MLC);
         |fprintf("\\n");
         |
         |dEpid_mm = $dEpid_mm;
         |fprintf("Average source to EPID mm: $f\\n", dEpid_mm);
         |
         |JawCenterX = ( Jaw_090_left_mm +  Jaw_090_right_mm + Jaw_270_left_mm +  Jaw_270_right_mm ) / 4.0;
         |JawCenterY = (  Jaw_090_top_mm + Jaw_090_bottom_mm +  Jaw_270_top_mm + Jaw_270_bottom_mm ) / 4.0;
         |MLCCenterX = ( MLC_090_left_mm +  MLC_090_right_mm + MLC_270_left_mm +  MLC_270_right_mm ) / 4.0;
         |MLCCenterY = (  MLC_090_top_mm + MLC_090_bottom_mm +  MLC_270_top_mm + MLC_270_bottom_mm ) / 4.0;
         |
         |fprintf("Jaw center X: $f\\n", JawCenterX);
         |fprintf("Jaw center Y: $f\\n", JawCenterY);
         |
         |fprintf("MLC center: $f\\n", MLCCenterX);
         |fprintf("MLC center: $f\\n", MLCCenterY);
         |
         |fprintf("\\n");
         |
         |aX = 1.0 / (((dEpid_mm - xJaw) / xJaw) - ((dEpid_mm - MLC) / MLC));
         |aY = 1.0 / (((dEpid_mm - yJaw) / yJaw) - ((dEpid_mm - MLC) / MLC));
         |
         |fprintf("aX: $f\\n", aX);
         |fprintf("aY: $f\\n", aY);
         |
         |focalSpotX = aX * ( JawCenterX - MLCCenterX );
         |focalSpotY = aY * ( JawCenterY - MLCCenterY );
         |fprintf("Focal spot: X: $f\\n", focalSpotX);
         |fprintf("Focal spot: Y: $f\\n", focalSpotY);
         |
         |% -------------------------------------------- Focal Spot Measurement for MV $mvText --------------------------------------------
         |""".stripMargin
    }

    // reduce multiple blank lines to single blank lines.
    focalSpotText
  }

}
