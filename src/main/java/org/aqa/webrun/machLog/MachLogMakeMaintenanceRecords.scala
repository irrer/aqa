package org.aqa.webrun.machLog

import edu.umro.ScalaUtil.Trace
import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.MachineLog
import org.aqa.db.MaintenanceRecord
import org.aqa.webrun.ExtendedData

import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Node
import scala.xml.XML

class MachLogMakeMaintenanceRecords(extendedData: ExtendedData, logList: Seq[MachineLog]) extends Logging {

  private abstract case class LogType(name: String) {

    /**
      * Re-format the node into a user-friendly format.
      * @param node Node to format.
      * @return User friendly text.
      */
    def describe(node: Node): String
  }

  private def formatParameter(parNode: Node): String = {
    val displayName = (parNode \ "@Display_Name").text
    val oldValue = (parNode \ "@Old_Value").text
    val newValue = (parNode \ "@New_Value").text
    /*
    val change = newValue - oldValue
    val percent = (change * 100) / oldValue
     */

    def dblOpt(text: String): Option[Double] = {
      if (text.matches(".*[T:tf,].*")) // disqualifies most non-numbers
        None
      else {
        try {
          Some(text.toDouble)
        } catch {
          case _: Throwable => None
        }
      }
    }

    val asXml: Option[String] = {
      if (oldValue.contains('<') && newValue.contains('>')) {
        try {
          def fmt(value: String, name: String): String = {
            val e = XML.loadString(value)
            val text = "      " + name + ": " + e.label + "  " + e.child.head.label + ": " +
              e.child.map(e => e.text).mkString("   ")
            text
          }

          val oldText = fmt(oldValue, "Old")
          val newText = fmt(newValue, "New")

          Some("\n" + oldText + "\n" + newText)
        } catch {
          case _: Throwable => None
        }
      } else
        None
    }

    val asDateTime: Option[String] = {
      def isDT(text: String): Option[Date] = {
        if ((text.length > 17) && (text(4) == '-') && (text(7) == '-') && (text(10) == 'T')) {
          val formatList = Seq(
            new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSSS-X"),
            new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSSSSS-X"),
            new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SS-X"),
            new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss-X"),
            new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
          )
          def toDate(fmt: SimpleDateFormat): Option[Date] = {
            try {
              Some(fmt.parse(text))
            } catch {
              case _: Throwable => None
            }
          }
          val date = formatList.flatMap(toDate).headOption
          date
        } else
          None
      }

      (isDT(oldValue), isDT(newValue)) match {
        case (Some(oldDT), Some(newDT)) =>
          val text = oldDT + " --> " + newDT + "    change: " + Util.elapsedTimeHumanFriendly(newDT.getTime - oldDT.getTime)
          Some(text)
        case _ => None
      }
    }

    val oldDouble = dblOpt(oldValue)
    val newDouble = dblOpt(newValue)

    val content = if (oldDouble.isDefined && newDouble.isDefined) {
      val o = oldDouble.get
      val n = newDouble.get
      val percent = ((o - n) * 100) / n
      val text = Util.fmtDbl(o) + " --> " + Util.fmtDbl(n) + "    change: " + Util.fmtDbl(o - n) + " = " + Util.fmtDbl(percent) + "%"
      text
    } else {
      (asXml, asDateTime) match {
        case (Some(xml), _) => xml
        case (_, Some(dt))  => dt
        case _              => " " + oldValue + " --> " + newValue // simplest case
      }
    }

    val text = "    " + displayName + ": " + content
    text
  }

  private def formatGroup(group: Node, prefix: String): String = {
    try {
      val name = (group \ "@name").text
      val subGroupList = group \ "Group"
      if (subGroupList.isEmpty)
        prefix + name + "\n" + (group \ "Parameter").map(p => formatParameter(p)).mkString("\n")
      else {
        val separator = if (prefix.trim.nonEmpty) " / " else ""
        val newPrefix = prefix + separator + name
        subGroupList.map(g => formatGroup(g, newPrefix)).mkString("\n")
      }
    } catch {
      case t: Throwable =>
        logger.warn("Unexpected exception auto-generating maintenance record: " + fmtEx(t) + "\n" + Util.prettyPrint(group))
        Util.prettyPrint(group)
    }
  }

  private def formatNode(node: Node): String = {
    val nodeName = (node \ "@name").text
    val text = nodeName + "\n" + (node \ "Group").map(g => formatGroup(g, "  ")).mkString("\n")
    Trace.trace("\n" + text)
    text
  }

  private def makeMaintenanceRecordsFromSingleMachineLog(machineLog: MachineLog): Seq[MaintenanceRecord] = {

    def makeOneRecord(logNode: Node): MaintenanceRecord = {
      val category = (logNode \ "@name").head.text

      val mr = new MaintenanceRecord(
        maintenanceRecordPK = None,
        category = category,
        machinePK = extendedData.machine.machinePK.get,
        creationTime = machineLog.DateTimeSaved,
        userPK = extendedData.user.userPK.get,
        outputPK = extendedData.output.outputPK,
        summary = category + " auto-generated",
        description = formatNode(logNode),
        machineLogPK = machineLog.machineLogPK
      )

      mr
    }

    val list = (machineLog.elem \ "Node").map(makeOneRecord)
    list
  }

  /**
    * Make MaintenanceRecords from the given list of MachineLogs.
    *
    * @return List of all maintenance records derived from the machine log list.
    */
  def makeMaintenanceRecords(): Seq[MaintenanceRecord] = {
    val allRec = logList.flatMap(makeMaintenanceRecordsFromSingleMachineLog)
    allRec
  }
}
