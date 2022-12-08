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

package learn

import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import edu.umro.ScalaUtil.DicomUtil
import org.aqa.AnonymizeUtil
import org.aqa.Util
import org.aqa.db.BBbyEPIDComposite
import org.aqa.db.BBbyEPIDComposite.getForOneDay
import org.aqa.db.Db
import org.aqa.db.Db.driver.api._
import org.aqa.db.DbSetup
import org.aqa.db.DicomSeries
import org.aqa.db.Machine
import org.aqa.db.Output
import org.aqa.webrun.dailyQA.DailyDataSetComposite

import java.util.Date
import scala.annotation.tailrec

/**
  * Gather timing information for how long it takes to run Daily QA.
  */
object DailyQaTiming {

  def dailyQaTiming(): Unit = {
    (0 to 10).foreach(_ => println())
    println("Starting --------------------------------------------------------------------------------------\n")

    def getInstPK: Long = {
      val action = for {
        comp <- BBbyEPIDComposite.query
        output <- Output.query.filter(o => o.outputPK === comp.outputPK)
        inst <- Machine.query.filter(m => output.machinePK === m.machinePK).map(_.institutionPK)
      } yield inst
      val list = Db.run(action.result)
      val instPK = list.groupBy(_ + 0).values.maxBy(_.size).head
      instPK
    }

    val firstDate = Util.standardDateFormat.parse("2021-05-01T12:00:00")
    val lastMs = (new Date).getTime

    val d24Hours = 24 * 60 * 60 * 1000.toLong

    val institutionPK = getInstPK // 1 for dev, 2 for PROD
    println("Using institutionPK: " + institutionPK)

    def getAcqTime(al: AttributeList): Date = {
      val date1 = DicomUtil.getTimeAndDate(al, TagFromName.AcquisitionDate, TagFromName.AcquisitionTime)
      val date2 = DicomUtil.getTimeAndDate(al, TagFromName.InstanceCreationDate, TagFromName.InstanceCreationTime)
      Seq(date1, date2).flatten.head
    }

    def show(dds: DailyDataSetComposite): Unit = {

      try {
        val cbctOutput = Output.get(dds.cbct.outputPK).get
        val epidOutput = Output.get(dds.bbByEpid.head.outputPK).get
        val epidSeries = DicomSeries.getBySopInstanceUID(dds.bbByEpid.head.epidSOPInstanceUid).head.attributeListList

        val epidTimeList = epidSeries.map(al => getAcqTime(al)).sortBy(_.getTime)

        val epidTimeFirst = epidTimeList.head
        val epidTimeLast = epidTimeList.last

        val cbctTimeList = dds.cbctDicomSeries.attributeListList.map(al => getAcqTime(al)).sortBy(_.getTime)
        val cbctTimeFirst = cbctTimeList.head
        val cbctTimeLast = cbctTimeList.last

        val startTime = cbctTimeFirst.getTime

        def elapsed(t: Date) = t.getTime - startTime

        // format elapsed time
        def fet(name: String, t: Date): String = {
          val sec = elapsed(t) / 1000
          val s = sec % 60
          val m = sec / 60

          "    " + name + ": " + m.formatted("%02d") + ":" + s.formatted("%02d")
        }

        val machId = AnonymizeUtil.decryptWithNonce(institutionPK, dds.machine.id_real.get)

        val pairList = Seq(
          ("firstCBCT", cbctTimeFirst),
          ("lastCBCT", cbctTimeLast),
          ("cbctAnal", cbctOutput.analysisDate.get),
          ("epidFirst", epidTimeFirst),
          ("epidLast", epidTimeLast),
          ("epidAnal", epidOutput.analysisDate.get),
          ("epidComp", dds.output.analysisDate.get)
        )

        val maxElapsed = pairList.map(p => elapsed(p._2)).max
        val tooLong = if (maxElapsed > d24Hours) "    Overnight: " + Util.elapsedTimeHumanFriendly(maxElapsed) else ""
        println("::::  " + machId + " " + Util.standardDateFormat.format(cbctTimeFirst) + pairList.map(p => fet(p._1, p._2)).mkString + tooLong)

        // @formatter:off
        // @formatter:on

      } catch {
        case _: Throwable =>
      }
    }

    @tailrec
    def doOneDay(date: Date): Unit = {
      val oneDay = getForOneDay(date, institutionPK)
      // only look at 'regular' sets, which means only one set of results for each machine, and exactly two EPID images for each result.  This eliminates other tests and experiments.
      val singleList = oneDay.groupBy(_.machine.machinePK.get).filter(group => group._2.size == 1).flatMap(_._2).filter(_.bbByEpid.size == 2)
      if (singleList.nonEmpty) {
        println(date)
        singleList.foreach(show)
      }
      if (date.getTime < lastMs) doOneDay(new Date(date.getTime + d24Hours))
    }

    doOneDay(firstDate)

    println("Done --------------------------------------------------------------------------------------\n")
  }

  def main(args: Array[String]): Unit = {
    DbSetup.init
    dailyQaTiming()
    System.exit(0)
  }
}
