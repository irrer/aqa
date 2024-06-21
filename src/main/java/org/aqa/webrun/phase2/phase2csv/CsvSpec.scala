package org.aqa.webrun.phase2.phase2csv

import org.aqa.db.Machine
import org.aqa.webrun.phase2.phase2csv.TimeComparator.TimeComparatorEnum

/**
  * Data model for custom CSV download parameters.
  */
object CsvSpec {

  val defaultTimeComparator: TimeComparator = TimeComparator(TimeComparatorEnum.TimeGE, TimeComparator.defaultDate)

  private val defaultCount = 1
  private val defaultSkip = 0
  case class CsvCount(count: Int = defaultCount, skip: Int = defaultSkip) {}

}

case class CsvSpec(machine: Machine, dataType: Phase2Csv[_], beam: String, header: Boolean, format: String, timeComparator: TimeComparator, count: CsvSpec.CsvCount) {
  def slice[T](list: Seq[T]): Seq[T] = {
    timeComparator.compare match {
      case TimeComparatorEnum.TimeLE => list.dropRight(count.skip).takeRight(count.count)
      case TimeComparatorEnum.TimeLT => list.dropRight(count.skip).takeRight(count.count)
      case _                         => list.slice(count.skip, count.skip + count.count)
    }
  }
}
