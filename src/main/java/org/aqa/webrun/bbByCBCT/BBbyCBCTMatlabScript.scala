package org.aqa.webrun.bbByCBCT

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import org.aqa.ImageRegistration
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.web.ViewOutput
import org.restlet.Response

import java.io.File
import java.text.SimpleDateFormat
import javax.vecmath.Point3d

/**
 * Create a Matlab script that will do the calculations.  This is for visibility for the user.
 */

class BBbyCBCTMatlabScript(output: Output, machine: Machine, cbctAnalysisResult: BBbyCBCTAnalysis.CBCTAnalysisResult, runReq: BBbyCBCTRunReq, response: Response) extends Logging {

  private val timeHumanFriendlyFormat = new SimpleDateFormat("EEE yyyy MM dd HH:mm:ss")

  private def point3dToText(p: Point3d) = p.getX + "  " + p.getY + "  " + p.getZ

  private val approximateLocation_vox = cbctAnalysisResult.coarseLocation_vox.getX + "  " + cbctAnalysisResult.coarseLocation_vox.getY + "  " + cbctAnalysisResult.coarseLocation_vox.getZ

  private val VoxelSize_mm = {
    val al = runReq.cbctList.head
    val PixelSpacing = al.get(TagByName.PixelSpacing).getDoubleValues
    val SliceThickness = al.get(TagByName.SliceThickness).getDoubleValues
    point3dToText(new Point3d(PixelSpacing.head, PixelSpacing(1), SliceThickness.head))
  }

  private def getIpp(al: AttributeList): String = {
    val ipp = al.get(TagByName.ImagePositionPatient).getDoubleValues
    point3dToText(new Point3d(ipp))
  }

  private val ImageOrientationPatient: String = {
    val iop = runReq.cbctList.head.get(TagByName.ImageOrientationPatient).getDoubleValues
    iop.take(3).mkString("  ") + ";\n    " + iop.slice(3, 6).mkString("  ")
  }

  def make(): Unit = {

    //noinspection SpellCheckingInspection
    val text =
      s"""
         |
         |% Calculate EPID results with Matlab code.  This code may be run with Matlab.  This code is provided as a convenience to
         |% allow users to ascertain that calculations are being done correctly.
         |%
         |% Note that the AQA generates these values using entirely different code (written in Scala), but because the answers match,
         |% users can be assured that the calculations are equivalent.  The only difference might be in round off errors, but the
         |% results will match to at least 10 significant figures.
         |%
         |% CBCT report: ${response.getRequest.getHostRef}${ViewOutput.viewOutputUrl(output.outputPK.get)}
         |% Data captured: ${timeHumanFriendlyFormat.format(output.dataDate.get)}
         |% Data analyzed: ${timeHumanFriendlyFormat.format(output.startDate)}
         |% Machine: ${machine.id}
         |%
         |% Approximate location of bb in voxel coordinates (not used in calculations).  This is an internal
         |% value generated by finding a small group of pixels with the greatest intensity as an
         |% approximation of the BB's location.  It is only shown here to aid with diagnosing image
         |% processing problems.
         |%
         |% approximateLocation_vox = [ $approximateLocation_vox ];
         |
         |% Precise location of bb in voxel coordinates found by creating a cubic spline in each of three different
         |% axis and finding the maximum point on each.
         |preciseLocation_vox = [ ${point3dToText(cbctAnalysisResult.fineLocation_vox)} ];
         |
         |% XYZ dimensions of a voxel in mm.  From PixelSpacing (0028,0030 ) and SliceThickness (0018,0050)
         |VoxelSize_mm = [ $VoxelSize_mm ];
         |
         |% Taken from the REG DICOM: FrameOfReferenceTransformationMatrix (3006,00C6 ).  Translates from
         |% CBCT space to RTPLAN space.
         |FrameOfReferenceTransformationMatrix = ${Util.formatMatrix(ImageRegistration(runReq.reg.get).getMatrix)}
         |
         |% ImagePositionPatient (0020,0032) from from each end (min and max Z) of the CBCT slices.
         |firstImagePositionPatient = [ ${getIpp(runReq.cbctList.head)}  ];
         |lastImagePositionPatient  = [ ${getIpp(runReq.cbctList.last)} ];
         |
         |% Number of slices in CBCT
         |numberOfSlices = ${runReq.cbctList.size.toString};
         |
         |% Isocenter of plan in RTPLAN frame of reference in mm
         |IsocenterPosition = [ ${point3dToText(Util.getPlanIsocenterList(runReq.rtplan).head)}];
         |
         |% ImageOrientationPatient (0020,0037) should be the same for all slices.  Note that if the table angle is
         |% zero (which is the expected case), then this will be:
         |%     1  0  0
         |%     0  1  0
         |ImageOrientationPatient = [
         |    $ImageOrientationPatient ];
         |
         |% Fraction of the volume in the Z direction where the BB was found.
         |%   e.g: If the BB was in the middle, then this would be 0.5
         |imageFraction = preciseLocation_vox(3) / (numberOfSlices - 1);
         |
         |% Perform a linear interpolation to find the ImagePositionPatient (0020,0032) at
         |% the BB's position.  This works because all of the translations are linear (no
         |% warping).  Also, the extreme ends of the CBCT are used to potentially get more
         |% significant digits, as opposed to slices that are closer together.
         |ImagePositionPatient = [
         |  (((lastImagePositionPatient(1) - firstImagePositionPatient(1)) * imageFraction) + firstImagePositionPatient(1))
         |  (((lastImagePositionPatient(2) - firstImagePositionPatient(2)) * imageFraction) + firstImagePositionPatient(2))
         |  (((lastImagePositionPatient(3) - firstImagePositionPatient(3)) * imageFraction) + firstImagePositionPatient(3)) ];
         |
         |
         |% Split into vectors.
         |xdir = ImageOrientationPatient(1,:)
         |ydir = ImageOrientationPatient(2,:)
         |
         |patientOrientationAndPosition = [
         |    xdir(1)*VoxelSize_mm(1)  ydir(1)*VoxelSize_mm(2)  0  ImagePositionPatient(1);
         |    xdir(2)*VoxelSize_mm(1)  ydir(2)*VoxelSize_mm(2)  0  ImagePositionPatient(2);
         |    xdir(3)*VoxelSize_mm(1)  ydir(3)*VoxelSize_mm(2)  0  ImagePositionPatient(3);
         |    0                        0                        0  1 ];
         |
         |% Location of BB as a vector in voxels in the CBCT space.
         |bbLocation_vox = [
         |    preciseLocation_vox(1);
         |    preciseLocation_vox(2);
         |    0;
         |    1]
         |
         |bbPostionInRtplan_mm = FrameOfReferenceTransformationMatrix * patientOrientationAndPosition * bbLocation_vox;
         |
         |% XYZ error of CBCT - PLAN isocenter
         |XYZerror = bbPostionInRtplan_mm(1:3) - IsocenterPosition';
         |fprintf("    XYZ error:  %f  %f  %f\\n", XYZerror(1),XYZerror(2),XYZerror(3));
         |
         |""".stripMargin

    val file = new File(output.dir, BBbyCBCTHTML.matlabFileName)
    logger.info("Writing matlab code file: " + file.getAbsolutePath + "\n" + text)
    Util.writeFile(file, text)
  }

}
