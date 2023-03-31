/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.aqa.webrun.phase2

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.AttributeTag
import com.pixelmed.dicom.TagFromName
import com.pixelmed.dicom.TimeAttribute
import edu.umro.DicomDict.TagByName
import edu.umro.ImageUtil.DicomImage
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.Logging
import org.aqa.Util
import org.aqa.run.ProcedureStatus
import org.aqa.Config
import org.aqa.db.BadPixel
import org.aqa.web.DicomAccess
import org.aqa.web.WebUtil
import org.aqa.webrun.ExtendedData

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Elem

/**
  * Store bad pixels in the database and generate HTML.
  */
object BadPixelAnalysis extends Logging {
  val fileName = "ViewDicom.html"

  /**
    * Validate the given data, and, if it is valid, return None, else return a message indicating the problem.
    */
  def validate(runReq: RunReq): Option[String] = {
    if (runReq.flood == null) Some("Flood field is required for ") else None
  }

  /**
    * Store the bad pixels in the database and return the number of bad pixels.
    */
  private def storeToDb(extendedData: ExtendedData, runReq: RunReq): Seq[BadPixel] = {

    /**
      * Given a point, create CSV text showing values for the region around that point.
      */
    def makeCsv(bpX: Int, bpY: Int, dicomImage: DicomImage): String = {
      def fmtPoint(x: Int, y: Int): String = if (dicomImage.validPoint(x, y)) dicomImage.get(x, y).round.toString else "NA"
      def rangeOf(i: Int) = (-BadPixel.radius to BadPixel.radius).map(o => i + o)
      def fmtRow(y: Int) = rangeOf(bpX).map(x => fmtPoint(x, y)).mkString(",")
      rangeOf(bpY).map(y => fmtRow(y)).mkString("\n")
    }

    def beamToBadPixels(beamName: String, SOPInstanceUID: String, badPixels: Seq[DicomImage.PixelRating], originalImage: DicomImage): Seq[BadPixel] = {
      badPixels.map(bp => {
        val j = new BadPixel(None, extendedData.output.outputPK.get, bp.x, bp.y, SOPInstanceUID, beamName, makeCsv(bp.x, bp.y, originalImage))
        j
      })
    }

    val beamList = runReq.rtimageMap.keys.par
      .flatMap(beamName => {
        val SOPInstanceUID = runReq.rtimageMap(beamName).get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
        beamToBadPixels(beamName, SOPInstanceUID, runReq.derivedMap(beamName).badPixels, runReq.derivedMap(beamName).originalImage)
      })
      .toList

    val floodSOPInstanceUID = runReq.flood.get(TagFromName.SOPInstanceUID).getSingleStringValueOrNull
    val floodBadPixelList = beamToBadPixels(Config.FloodFieldBeamName, floodSOPInstanceUID, runReq.floodBadPixelList, runReq.floodOriginalImage)

    val list = beamList ++ floodBadPixelList
    BadPixel.insert(list)
    list
  }

  /**
    * Get the gantry and collimator angles in a human friendly format.  Angles are rounded to 90.  If either or
    * both of the angles are missing from the DICOM attribute list, then return an empty string in their place.
    */
  private def gantryCollAngles(al: AttributeList): String = {
    def getAngle(prefix: String, tag: AttributeTag): String = {
      val attr = al.get(tag)
      if (attr == null) ""
      else {
        val d = attr.getDoubleValues
        if (d.isEmpty) ""
        else prefix + Util.angleRoundedTo90(d.head).toString
      }
    }

    getAngle("G", TagByName.GantryAngle) + " " + getAngle("C", TagByName.BeamLimitingDeviceAngle).trim
  }

  /**
    * Make the DICOM files web viewable.
    */
  private val makeDicomViewsSync = 0
  private def makeDicomViews(extendedData: ExtendedData, runReq: RunReq, badPixelList: Seq[BadPixel]): Unit =
    makeDicomViewsSync.synchronized({
      val outputDir = extendedData.output.dir
      val smallImageWidth = 100.toString

      val viewDir = new File(extendedData.output.dir, "view")
      def dicomView(beamName: String): Option[String] = {
        logger.info("Making DICOM view for beam " + beamName)
        val rtimage = runReq.rtimageMap(beamName)
        val derived = runReq.derivedMap(beamName)
        val angles = gantryCollAngles(rtimage)

        val bufImage = derived.originalImage.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
        Config.applyWatermark(bufImage)
        //val url = WebServer.urlOfResultsFile(rtimage.file)
        val result = DicomAccess.write(
          rtimage,
          angles + " : " + beamName,
          viewDir,
          Phase2Util.dicomViewBaseName(beamName, rtimage, runReq.rtplan),
          Some(bufImage),
          Some(derived.originalImage),
          derived.badPixels
        )
        logger.info("Finished making DICOM view for beam " + beamName)
        result
      }

      // write the RTPLAN
      val planLink = Phase2Util.dicomViewHref(runReq.rtplan, extendedData, runReq)
      val rtPlanAl = runReq.rtplan
      val planBaseName = Phase2Util.dicomViewBaseName(runReq.beamNameOfAl(rtPlanAl), rtPlanAl, runReq.rtplan)
      DicomAccess.write(runReq.rtplan, "RTPLAN", viewDir, planBaseName, None, None, Seq[DicomImage.PixelRating]())

      //val floodLink = Phase2Util.dicomViewHref(runReq.flood, extendedData, runReq)
      val floodBufImage = runReq.floodOriginalImage.toDeepColorBufferedImage(Config.DeepColorPercentDrop)
      Config.applyWatermark(floodBufImage)
      val floodBaseName = Phase2Util.dicomViewBaseName(Config.FloodFieldBeamName, runReq.flood, runReq.rtplan)
      DicomAccess.write(runReq.flood, Config.FloodFieldBeamName, viewDir, floodBaseName, Some(floodBufImage), Some(runReq.floodOriginalImage), runReq.floodBadPixelList).get

      runReq.rtimageMap.keys.par.map(beamName => (beamName, dicomView(beamName).get)).toList.toMap

      def timeOf(al: AttributeList): Option[Long] = {
        val timeAttr = Seq(TagFromName.ContentTime, TagFromName.AcquisitionTime, TagFromName.InstanceCreationTime)
          .map(tag => al.get(tag))
          .filter(at => (at != null) && at.isInstanceOf[TimeAttribute] && at.asInstanceOf[TimeAttribute].getDoubleValues.nonEmpty)
          .map(at => at.asInstanceOf[TimeAttribute])
          .headOption

        timeAttr match {
          case Some(at) => DicomUtil.parseDicomTime(at.getStringValues().head)
          case _        => None
        }
      }

      def orderBeams(beamA: String, beamB: String): Boolean = {
        val alA = runReq.rtimageMap(beamA)
        val alB = runReq.rtimageMap(beamB)
        (timeOf(alA), timeOf(alB)) match {
          case (Some(tA), Some(tB)) => tA < tB
          case _                    => beamA.compareTo(beamB) < 0
        }
      }

      val elapsedTimeFormat = new SimpleDateFormat("m:ss")

      val allImageTimes = {
        val allAttrLists = Seq(runReq.flood) ++ runReq.derivedMap.values.map(_.attributeList)
        allAttrLists.flatMap(al => timeOf(al)).sorted
      }

      val earliestTime = allImageTimes.head

      def sortBeams = runReq.rtimageMap.keys.toList.sortWith(orderBeams)

      def beamRef(beamName: String, al: AttributeList): (Option[Long], Elem) = {
        def angleOf(tag: AttributeTag) = Util.angleRoundedTo90(al.get(tag).getDoubleValues.head).toString

        val imageTime = timeOf(al)
        val relativeTimeText = {
          imageTime match {
            case Some(t) => elapsedTimeFormat.format(new Date(t - earliestTime))
            case _       => ""
          }
        }

        val dicomHref = Phase2Util.dicomViewHref(al, extendedData, runReq)
        val pngHref = Phase2Util.dicomViewImageHref(al, extendedData, runReq)

        val collimatorCenteringBeamNameList: Seq[String] = {
          0 match {
            case _ if extendedData.procedure.isPhase2 => Config.collimatorCenteringPhase2List
            case _ if extendedData.procedure.isPhase3 => Config.collimatorCenteringPhase3List
            case _ =>
              logger.warn("Unexpected problem, unable to determine which collimator centering beam names to use.  Procedure should be Phase2 or Phase3, but is: " + extendedData.procedure.toString())
              Seq()
          }
        }

        val elem = {
          def boolToName(name: String, b: Boolean) = if (b) name else ""
          def isVmat(beamName: String): Boolean = {
            Config.VMATBeamPairList.flatMap(p => Seq(p.MLC, p.OPEN)).exists(n => n.equalsIgnoreCase(beamName))
          }

          val centerDoseBeamNameList = Util.makeCenterDoseBeamNameList(runReq.rtplan)
          val symFlatConstBeamNameList = Util.makeSymFlatConstBeamNameList(runReq.rtplan)

          <tr align="center">
          <td style="text-align: center;" title="Click for DICOM metadata"><a href={dicomHref}>{beamName}</a></td>
          <td style="text-align: center;" title="Click for full size image"><a href={Phase2Util.dicomViewImageHtmlHref(al, extendedData, runReq)}><img src={pngHref} width={smallImageWidth}/></a></td>
          <td style="text-align: center;" title="Gantry Angle deg">{angleOf(TagByName.GantryAngle)}</td>
          <td style="text-align: center;" title="Collimator Angle deg">{angleOf(TagByName.BeamLimitingDeviceAngle)}</td>
          <td style="text-align: center;" title="Collimator opening in CM">{Phase2Util.jawDescription(al, runReq.rtplan)}</td>
          <td style="text-align: center;" title="Time since first image capture (mm:ss)">{relativeTimeText}</td>
          <td style="text-align: center;" title="Collimator Centering">{
            boolToName("Col Cntr", collimatorCenteringBeamNameList.contains(beamName))
          }</td>
          <td style="text-align: center;" title="Center Dose">{boolToName("Cntr Dose", centerDoseBeamNameList.contains(beamName))}</td>
          <td style="text-align: center;" title="Collimator Position">{boolToName("Col Posn", Config.CollimatorPositionBeamList.exists(cp => cp.beamName.equalsIgnoreCase(beamName)))}</td>
          <td style="text-align: center;" title="Wedge">{boolToName("Wedge", Config.WedgeBeamList.exists(w => w.wedge.equals(beamName)))}</td>
          <td style="text-align: center;" title="Symmetry and Flatness">{boolToName("Sym+Flat+Const", symFlatConstBeamNameList.contains(beamName))}</td>
          <td style="text-align: center;" title="Leaf Position">{boolToName("Leaf Posn", Config.LeafPositionBeamNameList.contains(beamName))}</td>
          <td style="text-align: center;" title="VMAT">{boolToName("VMAT", isVmat(beamName))}</td>
        </tr>
        }
        (imageTime, elem)
      }

      val content = {

        val allElem = {
          val rtimgRef = sortBeams.map(beamName => beamRef(beamName, runReq.rtimageMap(beamName)))
          def cmpr(a: (Option[Long], Elem), b: (Option[Long], Elem)): Boolean = {
            (a._1, b._1) match {
              case (Some(ta), Some(tb)) => ta < tb
              case (Some(_), _)         => false
              case (_, Some(_))         => true
              case (_, _)               => true
            }
          }
          val list = {
            if (sortBeams.contains(Config.FloodFieldBeamName))
              rtimgRef
            else
              rtimgRef :+ beamRef(Config.FloodFieldBeamName, runReq.flood)
          }
          list.sortWith(cmpr).map(te => te._2)
        }

        val tableHead = {
          <thead>
          <tr>
            <th style="text-align: center;" title='Click to view DICOM metadata'>Beam<br/>Name</th>
            <th style="text-align: center;" title='Click to view image'>Image</th>
            <th style="text-align: center;" title='Gantry angle rounded to nearest 90 degrees'>Gantry Angle<br/>degrees</th>
            <th style="text-align: center;" title='Collimator angle rounded to nearest 90 degrees'>Collimator Angle<br/>degrees</th>
            <th style="text-align: center;" title='Width times height of field in cm'>Field Size<br/>cm</th>
            <th style="text-align: center;" title='Time since first image capture (mm:ss)'>Acquisition<br/>Time</th>
            <th style="text-align: center;" title='if used in Collimator Centering'>Collimator<br/>Centering</th>
            <th style="text-align: center;" title='if used in Center Dose'>Center<br/>Dose</th>
            <th style="text-align: center;" title='if used in Collimator Position'>Collimator<br/>Position</th>
            <th style="text-align: center;" title='if used in Wedge'>Wedge</th>
            <th style="text-align: center;" title='if used in Symmetry and Flatness'>Symmetry and Flatness</th>
            <th style="text-align: center;" title='if used in Leaf Position (Picket Fence)'>Leaf Position</th>
            <th style="text-align: center;" title='if used in VMAT (RapidArc)'>VMAT</th>
          </tr>
        </thead>
        }

        val table = {
          <table class="table table-bordered">
          {tableHead}
          {allElem}
        </table>
        }

        val imageCount = runReq.rtimageMap.size + 1 // beams plus flood
        val badPixelCount = badPixelList.size
        val distinctBadPixelCount = badPixelList.map(bp => (bp.x, bp.y)).distinct.size

        val elapsedTitle = "Elapsed time (mm:ss) on treatment machine between" + WebUtil.titleNewline + "time stamp of first and last images."
        val subTitle = {
          <table class="table table-responsive">
          <tr>
            <td>
              <a href={planLink}>View Plan</a>
            </td>
            <td>
              Images:{imageCount.toString}
            </td>
            <td title={elapsedTitle}>
              Delivery Time:{elapsedTimeFormat.format(new Date(allImageTimes.last - allImageTimes.head))}
            </td>
            <td>
              Total bad pixels:{badPixelCount.toString}
            </td>
            <td>
              Distinct bad pixels:{distinctBadPixelCount.toString}
            </td>
          </tr>
        </table>
        }

        val mainElem = {
          <div class="col-md-10 col-md-offset-1">
          {subTitle}
          {table}
        </div>
        }

        mainElem
      }

      val text = Phase2Util.wrapSubProcedure(extendedData, content, "View Dicom", ProcedureStatus.pass, None, runReq.rtimageMap)
      val file = new File(outputDir, fileName)
      Util.writeBinaryFile(file, text.getBytes)
    })

  private val subProcedureName = "Bad Pixel"

  case class BadPixelResult(sum: Elem, sts: ProcedureStatus.Value, resultList: Seq[BadPixel]) extends SubProcedureResult(sum, sts, subProcedureName)

  /**
    * Run the BadPixelAnalysis sub-procedure, save results in the database, return true for pass or false for fail.  For it to pass all images have to pass.
    */
  def runProcedure(extendedData: ExtendedData, runReq: RunReq): Either[Elem, BadPixelResult] = {
    try {
      logger.info("Starting analysis of BadPixel for machine " + extendedData.machine.id)
      val badPixelList = storeToDb(extendedData, runReq)
      logger.info("Finished analysis of BadPixel, generating Bad Pixel reports")
      makeDicomViews(extendedData, runReq, badPixelList)

      val maxAllowedBadPixels = ((runReq.floodOriginalImage.Rows * runReq.floodOriginalImage.Columns) / 1000000.0) * Config.MaxAllowedBadPixelsPerMillion
      val pass = {
        val badPerImage = badPixelList.groupBy(b => b.SOPInstanceUID).values.map(list => list.size)
        badPerImage.isEmpty || badPerImage.max <= maxAllowedBadPixels
      }
      val status = if (pass) ProcedureStatus.pass else ProcedureStatus.fail

      val summary = {
        val imageCount = runReq.rtimageMap.size + 1 // beams plus flood
        val badPixelCount = badPixelList.size
        val distinctBadPixelCount = badPixelList.map(bp => (bp.x, bp.y)).distinct.size

        val title = "Click to view and download DICOM files.  Images: " + imageCount + "  Bad pixels: " + badPixelCount + "  Distinct bad pixels: " + distinctBadPixelCount
        <div title={title}>
          <a href={fileName}>
            View DICOM
            <br/>
            <img src={if (pass) Config.passImageUrl else Config.failImageUrl} height="32"/>
          </a>
        </div>
      }

      val result = Right(BadPixelResult(summary, status, badPixelList))
      logger.info("Finished analysis of BadPixel for machine " + extendedData.machine.id)
      result
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected error in analysis of BadPixel: " + t + fmtEx(t))
        Left(Phase2Util.procedureCrash(subProcedureName))
    }

  }
}
