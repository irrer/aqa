package org.aqa.stats

import java.text.SimpleDateFormat

object AnUtil {

  /* Column index of the machine name. */
  val IndexMachine = 1

  val spaces = (0 until 100).map(_ => "          ").mkString("          ")

  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  val TagInstitution = "Institution"
  val TagMachine = "Machine"
  val TagAcquisition = "Acquisition"
  val TagAnalysis = "Analysis"
  val TagProcedure = "Procedure"

  val ignoreMachSet = Set(
    "MACH_67",
    "MACH_68",
    "MACH_69"
  )

  val ignoreColumnNameSet = Set(
    "inputPK",
    "outputPK",
    "Analysis",
    "Beam Name",
    "Beam Name MLC",
    "Beam Name Open",
    "Beam Number",
    "C 270 SeriesInstanceUID",
    "C 270 Software Version",
    "C 270 SOPInstanceUID",
    "C 90 Beam Number",
    "C 90 Collimator Angle",
    "C 90 Gantry Angle",
    "C 90 Operator",
    "C 90 PatientID",
    "C 90 PatientName",
    "Coll 270 Beam Name",
    "Coll 90 Beam Name",
    "Collimator Angle",
    "CollimatorAnglePlan - Image deg",
    "collimatorAnglePlan deg",
    "Created by User",
    "Description",
    "Effective Date",
    "Institution",
    "Gantry Angle",
    "Gantry Angle Plan deg",
    "gantryAnglePlan - Image deg",
    "Machine",
    "Machine Type",
    "Maintenance Category",
    "Maintenance Record Marker",
    "Open Beam Number",
    "Open Collimator Angle",
    "Open Gantry Angle",
    "Open Operator",
    "Open PatientID",
    "Open PatientName",
    "Open SeriesInstanceUID",
    "Open Software Version",
    "Open SOPInstanceUID",
    "Operator",
    "outputPK",
    "Pass",
    "PatientID",
    "PatientName",
    "Procedure",
    "SeriesInstanceUID",
    "Software Version",
    "SOPInstanceUID",
    "Status",
    "Summary",
    "Units",
    "Uploaded By",
    "URL"
  )

}
