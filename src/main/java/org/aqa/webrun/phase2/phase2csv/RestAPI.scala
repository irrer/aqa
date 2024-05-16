package org.aqa.webrun.phase2.phase2csv

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.aqa.web.WebUtil.ValueMapT
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet

import java.util.Date

class RestAPI extends Restlet with SubUrlRoot with Logging {

  private val machineTag = "machine"

  private val dataTypeTag = "type"

  private val beamTag = "beam"

  private val headerTag = "header"

  private val timeEQTag = "timeEQ"
  private val timeGETag = "timeGE"
  private val timeLETag = "timeLE"
  private val timeLTTag = "timeLT"
  private val timeGTTag = "timeGT"

  private val takeTag = "take"
  private val dropTag = "skip"

  private val dataTypeList = Seq(
    new CenterDoseCsv,
    new GapSkewCsv,
    new CollimatorCenteringCsv,
    new CollimatorPositionCsv,
    new FocalSpotCsv,
    new LeafPositionCsv,
    new MetadataCheckCsv,
    new SymmetryAndFlatnessCsv,
    new VMAT_T2_DR_GSCsv,
    new VMAT_T2_DG_RSCsv,
    new VMAT_T3MLCSpeedCsv,
    new WedgePointCsv,
    new WinstonLutzCsv
  )

  private val dataTypeChoiceList = dataTypeList.map(_.getDataName).mkString(", ")

  private case class ParameterSet() {}

  /*
  object SubUrl extends Enumeration {
    type SubUrl = Value

    val root: WebUtil.SubUrl.Value = Value("")
    val admin: WebUtil.SubUrl.Value = Value("admin")
    val run: WebUtil.SubUrl.Value = Value("run")
    val view: WebUtil.SubUrl.Value = Value("view")
    val doc: WebUtil.SubUrl.Value = Value("doc")

    def url(subUrl: SubUrl.Value, name: String): String = {
      ("/" + subUrl + "/" + name).replace("//", "/")
    }
  }
   */

  private object TimeComparatorEnum extends Enumeration {

    type TimeComparator = Value

    val EQ = Value
    val GE = Value
    val LE = Value
    val LT = Value
    val GT = Value
  }

  private case class TimeComparator(time: Date, compare: TimeComparatorEnum.Value) {

  }

  private case class Count(take: Int = 1, skip: Int = 0) {}

  private def getMachine(valueMap: ValueMapT): Either[String, Machine] = {
    if (valueMap.contains(machineTag)) {
      WebUtil.getUser(valueMap) match {
        case Some(user) =>
          val machineList = Machine.getForInstitution(user.institutionPK)
          val machineNameList = machineList.map(_.getRealId)
          val machine = machineList.find(m => m.getRealId.trim.equalsIgnoreCase(valueMap(machineTag).trim))
          if (machine.isDefined)
            Right(machine.get)
          else
            Left(s"""Could not find machine ${valueMap(machineTag)} . Valid choices are: ${machineNameList.mkString(" ")}""")
        case _ =>
          Left("Invalid user.")
      }
    } else
      Left(s"Parameter '$machineTag' not specified.")
  }

  private def getDataType(valueMap: ValueMapT): Either[String, Phase2Csv[_]] = {
    if (valueMap.contains(dataTypeTag)) {
      val name = valueMap(dataTypeTag).trim
      val dataType = dataTypeList.find(dt => dt.getDataName.trim.equalsIgnoreCase(name))
      if (dataType.isDefined)
        Right(dataType.get)
      else
        Left(s"""No such data type $name .  Choices are $dataTypeChoiceList""")
    } else
      Left(s"""No $dataTypeTag specified.  Choices are $dataTypeChoiceList""")
  }

  private def getBeam(valueMap: ValueMapT): Either[String, String] = {
    if (valueMap.contains(beamTag))
      Right(valueMap(beamTag))
    else
      Right(".*")
  }

  private def getHeader(valueMap: ValueMapT): Either[String, Boolean] = {
    if (valueMap.contains(headerTag)) {
      val trueList = Seq("true", "yes", "y", "1")
      val falseList = Seq("false", "no", "n", "0")

      val text = valueMap(headerTag).trim.toLowerCase()

      0 match {
        case _ if trueList.contains(text)  => Right(true)
        case _ if falseList.contains(text) => Right(false)
        case _                             => Left(s"""Invalid value for $headerTag .   Should be either ${trueList.head} or ${falseList.head} .""")
      }
    } else
      Right(false)
  }

  private def getTime(valueMap: ValueMapT): Either[String, TimeComparator] = {
    ???
  }

  private def getCount(valueMap: ValueMapT): Either[String, Count] = {
    ???
  }

  private def parse(valueMap: ValueMapT): ParameterSet = {

    val machine = getMachine(valueMap)

    val dataType = getDataType(valueMap)

    val beam = getBeam(valueMap)

    val header = getHeader(valueMap)

    val time = getTime(valueMap)

    val count = getCount(valueMap)

    ???
  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
