package org.aqa.webrun.phase2.phase2csv

import org.apache.logging.log4j.core.util.datetime.FixedDateFormat
import org.aqa.db.Machine
import org.aqa.Util
import org.aqa.webrun.phase2.phase2csv.CsvSpec.TimeComparator
import org.aqa.webrun.phase2.phase2csv.CsvSpec.TimeComparatorEnum

import java.text.SimpleDateFormat
import java.util.Date

/**
  * Data model for custom CSV download parameters.
  */
object CsvSpec {

  object TimeComparatorEnum extends Enumeration {
    val timeEQ: TimeComparatorEnum.Value = Value
    val timeGE: TimeComparatorEnum.Value = Value
    val timeLE: TimeComparatorEnum.Value = Value
    val timeLT: TimeComparatorEnum.Value = Value
    val timeGT: TimeComparatorEnum.Value = Value
  }

  object TimeComparator {
    val dateFormat = new SimpleDateFormat(FixedDateFormat.FixedFormat.ISO8601_BASIC_PERIOD.getPattern)
    val defaultDate: Date = dateFormat.parse("19700101T000000.000")
  }

  case class TimeComparator(compare: CsvSpec.TimeComparatorEnum.Value, time: Date) {
    override def toString: String = {
      compare.toString + " " + Util.formatDate(CsvSpec.TimeComparator.dateFormat, time)
    }

    private val time_ms = time.getTime

    def ok(date: Date): Boolean = {
      val t_ms = date.getTime
      compare match {
        case TimeComparatorEnum.timeEQ => t_ms == time_ms
        case TimeComparatorEnum.timeGE => t_ms >= time_ms
        case TimeComparatorEnum.timeLE => t_ms <= time_ms
        case TimeComparatorEnum.timeLT => t_ms < time_ms
        case TimeComparatorEnum.timeGT => t_ms > time_ms
      }
    }
  }

  val defaultTimeComparator: TimeComparator = TimeComparator(TimeComparatorEnum.timeGE, TimeComparator.defaultDate)

  private val defaultCount = 1
  private val defaultSkip = 0
  case class CsvCount(count: Int = defaultCount, skip: Int = defaultSkip) {}
}

case class CsvSpec(machine: Machine, dataType: Phase2Csv[_], beam: String, header: Boolean, timeComparator: TimeComparator, count: CsvSpec.CsvCount) {
  def slice[T](list: Seq[T]): Seq[T] = {
    timeComparator.compare match {
      case TimeComparatorEnum.timeLE => list.dropRight(count.skip).takeRight(count.count)
      case TimeComparatorEnum.timeLT => list.dropRight(count.skip).takeRight(count.count)
      case _ => list.slice(count.skip, count.skip + count.count)
    }
  }
}
