package org.aqa.webrun.phase2.phase2csv

import org.apache.logging.log4j.core.util.datetime.FixedDateFormat
import org.aqa.Util
import org.aqa.webrun.phase2.phase2csv.TimeComparator.TimeComparatorEnum
// import org.aqa.webrun.phase2.phase2csv.CsvSpec.TimeComparatorEnum.Value

import java.text.SimpleDateFormat
import java.util.Date
import scala.annotation.tailrec

object TimeComparator {
  val dateFormatList = Seq(
    new SimpleDateFormat(FixedDateFormat.FixedFormat.ISO8601_BASIC_PERIOD.getPattern),
    new SimpleDateFormat("yyyyMMdd'T'HHmmss"),
    new SimpleDateFormat("yyyyMMdd'T'HHmm")
  )

  object TimeComparatorEnum extends Enumeration {
    val TimeEQ: TimeComparatorEnum.Value = Value
    val TimeGE: TimeComparatorEnum.Value = Value
    val TimeLE: TimeComparatorEnum.Value = Value
    val TimeLT: TimeComparatorEnum.Value = Value
    val TimeGT: TimeComparatorEnum.Value = Value
  }

  val defaultDate: Date = Util.parseDate(dateFormatList.head, "19700101T000000.000")

  @tailrec
  def parseDate(text: String, index: Int = 0): Either[String, Date] = {

    try {
      val date = Util.parseDate(dateFormatList(index), text)
      Right(date)
    } catch {
      case _: Throwable =>
        if ((index + 1) >= dateFormatList.size) {
          val current = Util.formatDate(dateFormatList.head, new Date)
          Left(
            s"""Unable to parse date+time text '$text'  \n""" +
              s"""Date specification should include year,month,day of month, T, hour, minute, second, millisecond.\n""" +
              s"""For example, current date+time would be formated as: $current\n"""
          )
        } else
          parseDate(text, index + 1)
    }

  }

  val defaultTimeComparator: TimeComparator = TimeComparator(TimeComparatorEnum.TimeGE, TimeComparator.defaultDate)
}

case class TimeComparator(compare: TimeComparatorEnum.Value, time: Date) {
  override def toString: String = {
    compare.toString + " " + Util.formatDate(TimeComparator.dateFormatList.head, time)
  }

  private val time_ms = time.getTime

  def ok(date: Date): Boolean = {
    val t_ms = date.getTime
    compare match {
      case TimeComparatorEnum.TimeEQ => t_ms == time_ms
      case TimeComparatorEnum.TimeGE => t_ms >= time_ms
      case TimeComparatorEnum.TimeLE => t_ms <= time_ms
      case TimeComparatorEnum.TimeLT => t_ms < time_ms
      case TimeComparatorEnum.TimeGT => t_ms > time_ms
    }
  }
}
