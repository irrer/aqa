package org.aqa.webrun.phase2.phase2csv

import org.aqa.Logging
import org.aqa.db.Machine
import org.aqa.web.WebUtil
import org.aqa.web.WebUtil.SubUrlRoot
import org.aqa.web.WebUtil.getValueMap
import org.aqa.web.WebUtil.internalFailure
import org.aqa.web.WebUtil.ValueMapT
import org.aqa.Util
import org.aqa.webrun.phase2.phase2csv.CsvSpec.CsvCount
import org.aqa.webrun.phase2.phase2csv.CsvSpec.TimeComparator
import org.aqa.webrun.phase2.phase2csv.CsvSpec.TimeComparatorEnum
import org.restlet.Request
import org.restlet.Response
import org.restlet.Restlet
import org.restlet.data.Status

import java.net.URLDecoder

class CsvApi extends Restlet with SubUrlRoot with Logging {

  // HTML parameter tag names
  private val machineTag = "machine"

  private val dataTypeTag = "type"

  private val beamTag = "beam"

  private val headerTag = "header"

  private val countTag = "count"
  private val skipTag = "skip"

  /**
    * Get the machine to use.
    * @param valueMap HTML parameters.
    * @return Either machine or an error message.
    */
  private def getMachine(valueMap: ValueMapT): Either[String, Machine] = {
    val user = WebUtil.getUser(valueMap)
    val machineList: Seq[Machine] = if (user.isDefined) Machine.getForInstitution(user.get.institutionPK) else Seq()
    val machineNameList: Seq[String] = if (machineList.isEmpty) Seq() else machineList.map(_.getRealId)
    def choiceList: String = "Valid choices are:\n" + machineNameList.mkString("\n")
    if (valueMap.contains(machineTag)) {
      WebUtil.getUser(valueMap) match {
        case Some(user) =>
          val machine = machineList.find(m => m.getRealId.trim.equalsIgnoreCase(valueMap(machineTag).trim))
          if (machine.isDefined)
            Right(machine.get)
          else
            Left(s"""Could not find machine ${valueMap(machineTag)} . $choiceList""")
        case _ =>
          Left("Invalid user.")
      }
    } else
      Left(s"Parameter '$machineTag' not specified. $choiceList")
  }

  /**
    * Get the type of data to download.
    * @param valueMap HTML parameters.
    * @return Either type of data or an error message.
    */
  private def getDataType(valueMap: ValueMapT): Either[String, Phase2Csv[_]] = {

    val dataTypeChoiceList = Phase2Csv.dataTypeList.map(_.getDataName).mkString(", ")

    if (valueMap.contains(dataTypeTag)) {
      val name = {
        val n = valueMap(dataTypeTag).trim
        URLDecoder.decode(n, "UTF-8")
      }
      val dataType = Phase2Csv.dataTypeList.find(dt => dt.getDataName.trim.equalsIgnoreCase(name))
      if (dataType.isDefined)
        Right(dataType.get)
      else
        Left(s"""No such data type $name .  Choices are $dataTypeChoiceList""")
    } else
      Left(s"""No $dataTypeTag specified.  Choices are $dataTypeChoiceList""")
  }

  /**
    * Get the beam name pattern to restrict the CSV rows.
    * @param valueMap HTML parameters.
    * @return Either the user's pattern or the default.  If the tag was specieid
    */
  private def getBeam(valueMap: ValueMapT): Either[String, String] = {
    val containsTag = valueMap.contains(beamTag)
    (containsTag, containsTag && (valueMap(beamTag) != null) && valueMap(beamTag).nonEmpty) match {
      case (true, true) => Right(valueMap(beamTag))
      case (false, _)   => Right(".*")
      case _            => Left("Empty string given for beam.")
    }
  }

  /**
    * Get the header value (true to show headers).  If not specified then return default (false).
    * @param valueMap HTML parameters.
    * @return Either boolean or error message.
    */
  private def getHeader(valueMap: ValueMapT): Either[String, Boolean] = {
    if (valueMap.contains(headerTag)) {
      val trueText = "true"
      val falseText = "false"

      val text = valueMap(headerTag).trim

      0 match {
        case _ if trueText.equalsIgnoreCase(text)  => Right(true)
        case _ if falseText.equalsIgnoreCase(text) => Right(false)
        case _                                     => Left(s"""Invalid value for $headerTag .   Should be either ${trueText.head} or ${falseText.head} .""")
      }
    } else
      Right(false)
  }

  /**
    * Get the time specification.  If not specified then return default.
    * @param valueMap HTML parameters.
    * @return Either time spec or error message.
    */
  private def getTime(valueMap: ValueMapT): Either[String, CsvSpec.TimeComparator] = {
    val timeTagSet = CsvSpec.TimeComparatorEnum.values.map(_.toString.toLowerCase())
    val specifiedSet = valueMap.keySet.filter(t => timeTagSet.contains(t.toLowerCase()))

    specifiedSet.size match {

      case 0 => // No time given, so use default
        Right(CsvSpec.defaultTimeComparator)

      case 1 =>
        val j = CsvSpec.TimeComparatorEnum.values.toList
        val c = CsvSpec.TimeComparatorEnum.values.find(c => c.toString.equalsIgnoreCase(specifiedSet.head.toLowerCase())).get
        val t = Util.parseDate(CsvSpec.TimeComparator.dateFormat, valueMap(specifiedSet.head))
        Right(TimeComparator(c, t))

      case _ => // more than one time given - ambiguous
        val timeUsage = s"""To specify time, use one of: ${TimeComparatorEnum.values.mkString(", ")} with a time in standard date format.  Default: ${CsvSpec.TimeComparator} """
        Left(s"""More than one time specification given: ${specifiedSet.mkString(", ")} .   $timeUsage""")

    }
  }

  /**
    * Get the count specification.  If not specified then return default.
    * @param valueMap HTML parameters.
    * @return Either count spec or error message.
    */
  private def getCount(valueMap: ValueMapT): Either[String, CsvCount] = {

    val count: Either[String, Int] = {
      val key = valueMap.keys.find(k => k.equalsIgnoreCase(countTag.toLowerCase()))
      if (key.isDefined) {
        try {
          val i = valueMap(key.get).toInt
          Right(i)
        } catch {
          case _: Throwable => Left(s"""Invalid integer given for count: ${valueMap(key.get)}""")
        }
      } else
        Right(1)
    }

    val skip: Either[String, Int] = {
      val key = valueMap.keys.find(k => k.equalsIgnoreCase(skipTag.toLowerCase()))
      if (key.isDefined) {
        try {
          val i = valueMap(key.get).toInt
          Right(i)
        } catch {
          case _: Throwable => Left(s"""Invalid integer given for skip: ${valueMap(key.get)}""")
        }
      } else
        Right(0)
    }

    if (count.isRight && skip.isRight) {
      Right(CsvCount(count.right.get, skip.right.get))
    } else {
      val text = Seq(count, skip).filter(_.isLeft).map(_.left.get).mkString(", and also ")
      Left(text)
    }
  }

  /**
    * Given the parameters from the HTTP request, validate and parse them.
    *
    * If there is an error, then set the response appropriately.
    *
    * @param valueMap List of parameters.
    * @param response Report errors here.
    * @return Specification for getting CSV rows, or None on error.
    */
  private def parse(valueMap: ValueMapT, response: Response): Either[String, CsvSpec] = {

    val machine = getMachine(valueMap)

    val dataType = getDataType(valueMap)

    val beam = getBeam(valueMap)

    val header = getHeader(valueMap)

    val timeComparator = getTime(valueMap)

    val count = getCount(valueMap)

    val errorList = Seq(machine, dataType, beam, header, timeComparator, count).filter(_.isLeft)

    if (errorList.isEmpty) {
      Right(CsvSpec(machine.right.get, dataType.right.get, beam.right.get, header.right.get, timeComparator.right.get, count.right.get))
    } else {
      val errorText = errorList.filter(_.isLeft).map(_.left.get).mkString("\n").toString
      Left(errorText)
    }

  }

  override def handle(request: Request, response: Response): Unit = {
    try {
      super.handle(request, response)
      val valueMap = getValueMap(request)
      val csvSpec = parse(valueMap, response)

      if (csvSpec.isRight)
        CsvSpecDownload.download(csvSpec.right.get, response)
      else {
        // show error to user
        val errorText = csvSpec.left.get
        logger.info(s"Bad request from user: ${request.toString} :: $errorText")
        WebUtil.badRequest(response, errorText, Status.CLIENT_ERROR_BAD_REQUEST)
      }

    } catch {
      case t: Throwable =>
        internalFailure(response, t)
    }
  }
}
