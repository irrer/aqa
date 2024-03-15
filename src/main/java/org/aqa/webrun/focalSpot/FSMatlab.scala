package org.aqa.webrun.focalSpot

import edu.umro.DicomDict.TagByName
import org.aqa.Config
import org.aqa.db.FocalSpot

import java.util.Date

object FSMatlab {

  // format all numbers like this
  private val f = "%20.16f"

  private def prelude(fsSet: FSSet) = {
    val id = ("MV: " + fsSet.mvText + "    Generated by AQA " + new Date() + Seq.fill(200)(" ").mkString).take(63) + "|"
    s"""
       |%----------------------------------------------------------------+
       |%                                                                |
       |%                   Focal Spot Matlab Code                       |
       |% $id
       |%                                                                |
       |% The following Matlab source code demonstrates how the          |
       |% calculations for focal spot are performed.                     |
       |%                                                                |
       |% The user may copy and paste this code into Matlab and run it   |
       |% to understand the process and verify correctness.              |
       |%                                                                |
       |% The code starts with the pixel coordinates of the 16 edges     |
       |% and converts these to coordinates in the isoplane.  Much of    |
       |% the code is dedicated to this conversion.  The final portion   |
       |% of the code takes the isoplane values and produces the focal   |
       |% spot values.                                                   |
       |%                                                                |
       |% This particular instance has been generated for this set of    |
       |% data, with all setup parameters and DICOM values filled in.    |
       |% Making this code available is a convenience for users who      |
       |% want to verify and understand how the final values are found.  |
       |%                                                                |
       |% The AQA performs equivalent calculations to the ones in the    |
       |% generated code.  The AQA service does not invoke the Matlab    |
       |% code.  The two different ways produce the same answers, but    |
       |% may in some cases differ slightly in the least significant     |
       |% digits due to round offs during calculation.  The fact that    |
       |% they do agree to 15 significant figures gives a high degree    |
       |% of confidence that they are equivalent.                        |
       |%                                                                |
       |%----------------------------------------------------------------+
       |""".stripMargin
  }

  private def focalSpotEdgeMatlabText(fsMeasure: FSMeasure): String = {

    val mvText = fsMeasure.mvText

    val typeOf = if (fsMeasure.isMLC) "MLC" else "Jaw"
    val prefix = typeOf + "_" + fsMeasure.collimatorAngleRounded_deg.formatted("%03d") + "_"

    // val translation = fsMeasure.rtimage.get(TagByName.XRayImageReceptorTranslation).getDoubleValues.toSeq
    val RTImageSID = FocalSpot.roundRTImageSID(fsMeasure.rtimage.get(TagByName.RTImageSID).getDoubleValues.head)
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
             |""".stripMargin
        } else {
          val RTImagePosition = RTImagePositionAttr.getDoubleValues
          s"""
             |% DICOM 3002,0012 RTImagePosition : RT Image Plane, Position and Orientation (in mm) : https://dicom.innolitics.com/ciods/rt-image/rt-image/30020012
             |${prefix}imagePosX = ${RTImagePosition.head} / ${prefix}beamExpansion;
             |${prefix}imagePosY = ${RTImagePosition(1)} / ${prefix}beamExpansion;
             |fprintf("\\n");
             |""".stripMargin
        }
      }

      // print the results for imagePosX and imagePosY
      val printText = {
        s"""
           |fprintf("${prefix}imagePosX: $f\\n", ${prefix}imagePosX);
           |fprintf("${prefix}imagePosY: $f\\n", ${prefix}imagePosY);
           |fprintf("\\n");
           |""".stripMargin
      }
      calcText + printText
    }

    val measureText: String = {
      s"""
         |fprintf("------------------------ Begin Focal Spot Edge Measurements for MV $mvText ${fsMeasure.focalSpot.beamLimiterName} ${fsMeasure.collimatorAngleRounded_deg} ------------------------\\n");
         |% This section converts edge measurements in pixels to mm in the isoplane, and is repeated for each of the four beams.
         |% A summary of the edge values and the final calculations for focal spot are at the end of this file.
         |
         |fprintf("Matlab code calculating edge positions used for focal spot calculation for $typeOf ${fsMeasure.collimatorAngleRounded_deg} for MV $mvText\\n");
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
         |${prefix}top_mm    = ( ${prefix}top_px    / ${prefix}expandY ) - ${prefix}imagePosY;
         |${prefix}bottom_mm = ( ${prefix}bottom_px / ${prefix}expandY ) - ${prefix}imagePosY;
         |${prefix}left_mm   = ( ${prefix}left_px   / ${prefix}expandX ) + ${prefix}imagePosX;
         |${prefix}right_mm  = ( ${prefix}right_px  / ${prefix}expandX ) + ${prefix}imagePosX;
         |
         |fprintf("$prefix calculated top, bottom, right, left mm:    $f    $f    $f    $f\\n", ${prefix}top_mm, ${prefix}bottom_mm, ${prefix}left_mm, ${prefix}right_mm);
         |fprintf("\\n");
         |
         |fprintf("Edge results calculated by AQA that were stored in the database (for verification).\\n");
         |fprintf("$prefix AQA values top, bottom, right, left mm:    $f    $f    $f    $f\\n", ${fsMeasure.focalSpot.topEdge_mm}, ${fsMeasure.focalSpot.bottomEdge_mm}, ${fsMeasure.focalSpot.leftEdge_mm}, ${fsMeasure.focalSpot.rightEdge_mm});
         |fprintf("\\n");
         |
         |fprintf("------------------------ End Focal Spot Edge Measurements for MV $mvText ${fsMeasure.focalSpot.beamLimiterName} ${fsMeasure.collimatorAngleRounded_deg} ------------------------\\n");
         |""".stripMargin
    }

    measureText
  }

  private def focalSpotAggregateMatlabText(fsSet: FSSet): String = {

    val mvText = fsSet.mvText

    // calculate the average epid to source value
    val dEpid_mm: Double = (fsSet.jaw090.dEpid_mm + fsSet.jaw270.dEpid_mm + fsSet.mlc090.dEpid_mm + fsSet.mlc270.dEpid_mm) / 4.0
    val dIso_mm: Double = (fsSet.jaw090.dIso_mm + fsSet.jaw270.dIso_mm + fsSet.mlc090.dIso_mm + fsSet.mlc270.dIso_mm) / 4.0

    val focalSpotText = {
      s"""
         |fprintf("------------------------ Begin summary of edge measurements for MV $mvText ------------------------\\n");
         |
         |fprintf("\\nImage plane measurements in pixels:\\n");
         |fprintf("Jaw 090 top bottom right left pix:  $f    $f    $f    $f\\n", Jaw_090_top_px, Jaw_090_bottom_px, Jaw_090_left_px, Jaw_090_right_px);
         |fprintf("Jaw 270 top bottom right left pix:  $f    $f    $f    $f\\n", Jaw_270_top_px, Jaw_270_bottom_px, Jaw_270_left_px, Jaw_270_right_px);
         |fprintf("MLC 090 top bottom right left pix:  $f    $f    $f    $f\\n", MLC_090_top_px, MLC_090_bottom_px, MLC_090_left_px, MLC_090_right_px);
         |fprintf("MLC 270 top bottom right left pix:  $f    $f    $f    $f\\n", MLC_270_top_px, MLC_270_bottom_px, MLC_270_left_px, MLC_270_right_px);
         |
         |fprintf("\\nIsoplane measurements in mm:\\n");
         |fprintf("Jaw 090 top bottom right left mm:  $f    $f    $f    $f\\n", Jaw_090_top_mm, Jaw_090_bottom_mm, Jaw_090_left_mm, Jaw_090_right_mm);
         |fprintf("Jaw 270 top bottom right left mm:  $f    $f    $f    $f\\n", Jaw_270_top_mm, Jaw_270_bottom_mm, Jaw_270_left_mm, Jaw_270_right_mm);
         |fprintf("MLC 090 top bottom right left mm:  $f    $f    $f    $f\\n", MLC_090_top_mm, MLC_090_bottom_mm, MLC_090_left_mm, MLC_090_right_mm);
         |fprintf("MLC 270 top bottom right left mm:  $f    $f    $f    $f\\n", MLC_270_top_mm, MLC_270_bottom_mm, MLC_270_left_mm, MLC_270_right_mm);
         |
         |topPlanned    = -50.0;
         |bottomPlanned =  50.0;
         |leftPlanned   = -50.0;
         |rightPlanned  =  50.0;
         |
         |fprintf("\\nIsoplane measurements in mm difference from plan:\\n");
         |fprintf("Jaw 090 measured-planned top bottom right left mm:  $f    $f    $f    $f\\n", Jaw_090_top_mm - topPlanned, Jaw_090_bottom_mm - bottomPlanned, Jaw_090_left_mm - leftPlanned, Jaw_090_right_mm - rightPlanned);
         |fprintf("Jaw 270 measured-planned top bottom right left mm:  $f    $f    $f    $f\\n", Jaw_270_top_mm - topPlanned, Jaw_270_bottom_mm - bottomPlanned, Jaw_270_left_mm - leftPlanned, Jaw_270_right_mm - rightPlanned);
         |fprintf("MLC 090 measured-planned top bottom right left mm:  $f    $f    $f    $f\\n", MLC_090_top_mm - topPlanned, MLC_090_bottom_mm - bottomPlanned, MLC_090_left_mm - leftPlanned, MLC_090_right_mm - rightPlanned);
         |fprintf("MLC 270 measured-planned top bottom right left mm:  $f    $f    $f    $f\\n", MLC_270_top_mm - topPlanned, MLC_270_bottom_mm - bottomPlanned, MLC_270_left_mm - leftPlanned, MLC_270_right_mm - rightPlanned);
         |
         |fprintf("\\n------------------------ End summary of edge measurements for MV $mvText ------------------------\\n");
         |
         |fprintf("------------------------ Begin Focal Spot Calculations for MV $mvText ------------------------\\n");
         |% At this point the 16 edge values have been converted from pixel coordinates to
         |% ISO plane coordinates.  The following combines them with the source to jaw and
         |% source to imager distances to produce the focal spot offset.
         |
         |% Note that all values are in mm.
         |
         |% distance in mm from radiation source to X jaw, Y jaw, and collimator.
         |xJaw = ${Config.TrueBeamSourceToXJawDistance_mm};
         |yJaw = ${Config.TrueBeamSourceToYJawDistance_mm};
         |MLC  = ${Config.TrueBeamSourceToMLCDistance_mm};
         |
         |fprintf("xJaw = $f;  %% Source to X jaws mm\\n", xJaw);
         |fprintf("yJaw = $f;  %% Source to Y jaws mm\\n", yJaw);
         |fprintf("MLC  = $f;  %% Source to MLC    mm\\n", MLC);
         |fprintf("\\n");
         |
         |dEpid_mm = $dEpid_mm;
         |dIso_mm = $dIso_mm;
         |fprintf("dEpid_mm = $f  %% Average source to EPID mm\\n", dEpid_mm);
         |fprintf("dIso_mm  = $f  %% Average source to  ISO mm\\n", dIso_mm);
         |
         |JawCenterX = ( Jaw_090_left_mm + Jaw_090_right_mm  + Jaw_270_left_mm + Jaw_270_right_mm  ) / 4.0;
         |JawCenterY = ( Jaw_090_top_mm  + Jaw_090_bottom_mm + Jaw_270_top_mm  + Jaw_270_bottom_mm ) / 4.0;
         |MLCCenterX = ( MLC_090_left_mm + MLC_090_right_mm  + MLC_270_left_mm + MLC_270_right_mm  ) / 4.0;
         |MLCCenterY = ( MLC_090_top_mm  + MLC_090_bottom_mm + MLC_270_top_mm  + MLC_270_bottom_mm ) / 4.0;
         |
         |fprintf("JawCenterX = $f\\n", JawCenterX);
         |fprintf("JawCenterY = $f\\n", JawCenterY);
         |
         |fprintf("MLCCenterX = $f\\n", MLCCenterX);
         |fprintf("MLCCenterY = $f\\n", MLCCenterY);
         |
         |fprintf("\\n");
         |
         |% aX = 1.0 / (((dEpid_mm - xJaw) / xJaw) - ((dEpid_mm - MLC) / MLC));
         |% aY = 1.0 / (((dEpid_mm - yJaw) / yJaw) - ((dEpid_mm - MLC) / MLC));
         |
         |fprintf("aX = 1.0 / (((dIso_mm - xJaw) / xJaw) - ((dIso_mm - MLC) / MLC))\\n");
         |fprintf("aX = 1.0 / ((($dIso_mm - %f) / %f) - (($dIso_mm - %f) / %f))\\n", xJaw, xJaw, MLC, MLC);
         |aX = 1.0 / (((dIso_mm - xJaw) / xJaw) - ((dIso_mm - MLC) / MLC));
         |fprintf("aX = $f\\n", aX);
         |fprintf("\\n");
         |fprintf("aY = 1.0 / (((dIso_mm - yJaw) / yJaw) - ((dIso_mm - MLC) / MLC))\\n");
         |fprintf("aY = 1.0 / ((($dIso_mm - %f) / %f) - (($dIso_mm - %f) / %f))\\n", yJaw, yJaw, MLC, MLC);
         |aY = 1.0 / (((dIso_mm - yJaw) / yJaw) - ((dIso_mm - MLC) / MLC));
         |fprintf("aY = $f\\n", aY);
         |
         |fprintf("\\n");
         |
         |fprintf("focalSpotX = aX * ( JawCenterX - MLCCenterX )\\n");
         |fprintf("focalSpotX = $f * ( $f - $f )\\n", aX, JawCenterX, MLCCenterX);
         |focalSpotX = aX * ( JawCenterX - MLCCenterX );
         |fprintf("focalSpotX = $f\\n", focalSpotX);
         |
         |fprintf("\\n");
         |
         |fprintf("focalSpotY = aY * ( JawCenterY - MLCCenterY )\\n");
         |fprintf("focalSpotY = $f * ( $f - $f )\\n", aY, JawCenterY, MLCCenterY);
         |focalSpotY = aY * ( JawCenterY - MLCCenterY );
         |fprintf("focalSpotY = $f\\n", focalSpotY);
         |fprintf("\\n");
         |
         |fprintf("Focal spot calculated from Matlab code: X: $f    Y: $f\\n", focalSpotX, focalSpotY);
         |
         |fprintf("Focal spot retrieved from AQA database: X: $f    Y: $f\\n", ${fsSet.focalSpotAlignmentX_mm}, ${fsSet.focalSpotAlignmentY_mm});
         |
         |fprintf("------------------------ End Focal Spot Measurement for MV $mvText ------------------------\\n");
         |""".stripMargin
    }

    // reduce multiple blank lines to single blank lines.
    focalSpotText
  }

  def generateMatlabCode(fsSet: FSSet): String = {
    val code = prelude(fsSet) + Seq(fsSet.jaw090, fsSet.jaw270, fsSet.mlc090, fsSet.mlc270).map(focalSpotEdgeMatlabText).mkString("\n") + focalSpotAggregateMatlabText(fsSet)
    code
  }

}
