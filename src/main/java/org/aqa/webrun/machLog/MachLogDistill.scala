package org.aqa.webrun.machLog

import org.aqa.Logging
import org.aqa.Util
import org.aqa.db.MachineLog
import org.aqa.db.MaintenanceRecord

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.Node
import scala.xml.NodeSeq
import scala.xml.XML

object MachLogDistill extends Logging {

  private val rtArrow = " --> "

  /** If both the old and new values are less than this many characters, then put them on one line. */
  private val oneLine = 50

  private val sp2 = "  "
  private val sp4 = sp2 + sp2
  private val sp6 = sp4 + sp2

  private abstract case class LogType(name: String) {

    /**
      * Re-format the node into a user-friendly format.
      * @param node Node to format.
      * @return User friendly text.
      */
    def describe(node: Node): String
  }

  private def formatParameter(parNode: Node): String = {
    val displayName = {
      if ((parNode \ "@Display_Name").nonEmpty)
        (parNode \ "@Display_Name").text
      else
        (parNode \ "@Enum_Name").text
    }
    val oldValue = (parNode \ "@Old_Value").text
    val newValue = (parNode \ "@New_Value").text

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
          //noinspection SpellCheckingInspection
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
          val text = oldDT + rtArrow + newDT + "    change: " + Util.elapsedTimeHumanFriendly(newDT.getTime - oldDT.getTime)
          Some(text)
        case _ => None
      }
    }

    val oldDouble = dblOpt(oldValue)
    val newDouble = dblOpt(newValue)

    val content: String = if (oldDouble.isDefined && newDouble.isDefined) {
      val o = oldDouble.get
      val n = newDouble.get
      val percent: String = {
        (o, n) match {
          case (0.0, 0.0) => " = 0%"
          case (0.0, _)   => ""
          case (_, 0.0)   => ""
          case _ =>
            val pct = ((n - o) * 100) / o
            " = " + Util.fmtDbl(pct) + "%"
        }
      }
      val change = "    change: " + Util.fmtDbl(o - n) + percent

      val text = displayName + ": " + Util.fmtDbl(o) + rtArrow + Util.fmtDbl(n) + change
      text
    } else {
      (asXml, asDateTime) match {
        case (Some(xml), _) => displayName + ": " + xml
        case (_, Some(dt))  => displayName + ": " + dt
        case _ =>
          if ((oldValue.length < oneLine) && (oldValue.length < oneLine))
            displayName + ": " + oldValue + rtArrow + newValue
          else
            displayName +
              "\n" + sp6 + "Old: " + oldValue +
              "\n" + sp6 + "New: " + newValue
      }
    }

    content
  }

  /**
    * Describe a list of nodes within a machine log.
    * @param node MachineLog Node being represented.
    * @param groupList List of Group nodes.
    */
  private case class LogGroup(node: Node, groupList: Seq[Node]) {

    private def nameOf(n: Node) = (n \ "@name").text

    val groupNames: String = groupList.map(nameOf).mkString("%%%%")

    val parameterList: NodeSeq = groupList.last \ "Parameter"

    override def toString: String = {
      nameOf(node) + "::::" + groupNames
    }
  }

  /**
    * Given a Node (with tag Node in machine log), return a list of all of the groups within that
    * Node.
    *
    * <p/>
    * IMHO: It is unfortunate that Varian picked such an overloaded term as Node, but that is what
    * we are stuck with.
    *
    * @param node Node (eg: <Node name="Beam Generation Module">)
    * @return List of groups within the Node.
    */
  private def findGroups(node: Node): Seq[LogGroup] = {
    val groupList = node \ "Group"

    def checkGroup(nodeList: Seq[Node], logList: Seq[LogGroup] = Seq()): Seq[LogGroup] = {
      def hasParam(node: Node) = (node \ "Parameter").nonEmpty

      if (hasParam(nodeList.last)) {
        val lg = LogGroup(node, nodeList)
        logList :+ lg
      } else {
        val gl = (nodeList.last \ "Group").map(g => checkGroup(nodeList :+ g, logList))
        gl.flatten
      }

    }

    groupList.flatMap(g => checkGroup(g))
  }

  private def formatGroup(logGroup: LogGroup): String = {
    try {
      logGroup.groupNames + "\n" + sp4 + logGroup.parameterList.map(formatParameter).mkString("\n" + sp4)
    } catch {
      case t: Throwable =>
        val text = logGroup.toString + "\n" + Util.prettyPrint(logGroup.groupList.last)
        logger.warn("Unexpected exception auto-generating maintenance record: " + fmtEx(t) + "\n" + text)
        text
    }
  }

  /**
    * Sometimes parameters are direct children of the Node.  If they exist, format them.
    * @param node the Node node.
    * @return Text description.  Will usually be an empty string.
    */
  private def formatNodeParameters(node: Node): String = {
    val nodeParameterList = (node \ "Parameter").map(formatParameter)
    if (nodeParameterList.isEmpty)
      ""
    else
      "\n" + sp2 + nodeParameterList.mkString("\n" + sp2)
  }

  private def formatNode(machineLog: MachineLog, node: Node): String = {
    val groupList = findGroups(node)
    val header =
      (node \ "@name").text + sp4 + " System Version: " + machineLog.SystemVersion

    val text = header + formatNodeParameters(node) + groupList.map(formatGroup).mkString("\n" + sp2, "\n" + sp2, "")
    text
  }

  private def formatNode(machineLog: MachineLog, machineLogNodeIndex: Long): String = {
    val node = (machineLog.elem \ "Node")(machineLogNodeIndex.toInt)
    formatNode(machineLog, node)
  }

  private def formatSummary(node: Node): String = {
    val groups = findGroups(node)
    val nodeName = (node \ "@name").text

    def showGroup(g: LogGroup): Unit = {
      val parameterTextList = g.parameterList.map(p => (p \ "@Enum_Name").text).sorted
      parameterTextList.map(p => println("== " + nodeName + "||||" + g.groupNames + "====" + p))
    }

    groups.map(showGroup)

    if (groups.nonEmpty) {
      groups.head.groupNames
    } else
      (node \ "@name").head.text
  }

  private def makeMachineRecord(machineLog: MachineLog, machineLogNodeIndex: Long, userPK: Long, outputPK: Long): MaintenanceRecord = {
    val node = (machineLog.elem \ "Node")(machineLogNodeIndex.toInt)
    val category = (node \ "@name").head.text
    val mr = new MaintenanceRecord(
      maintenanceRecordPK = None,
      category = category,
      machinePK = machineLog.machinePK,
      creationTime = machineLog.DateTimeSaved,
      userPK = userPK,
      outputPK = Some(outputPK),
      machineLogPK = machineLog.machineLogPK,
      machineLogNodeIndex = Some(machineLogNodeIndex),
      summary = formatSummary(node),
      description = formatNode(machineLog, machineLogNodeIndex)
    )

    mr
  }

  /**
    * Given a machine log, create one or more maintenance records.
    * @param machineLog Contains machine log XML.
    * @param userPK Created by this user.
    * @param outputPK References this output.
    * @return A list of maintenance records.
    */
  def makeMaintenanceRecordList(machineLog: MachineLog, userPK: Long, outputPK: Long): Seq[MaintenanceRecord] = {
    val nodeList = machineLog.elem \ "Node"
    val list = nodeList.zipWithIndex.map(li => makeMachineRecord(machineLog, li._2, userPK, outputPK))
    list
  }

  def main(args: Array[String]): Unit = {
    //val dir = new File("""D:\tmp\aqa\MachineLogs\CedarsSinia""")
    val dir = new File("""D:\tmp\aqa\MachineLogs\H192448\H192448""")
    val fileList = Util.listDirFiles(dir).filter(_.getName.startsWith("SavedConfigParameters_"))
    val machLogList = fileList.flatMap(file => {
      try {
        // println("\nfile: " + file.getName)
        val text = Util.readTextFile(file).right.get.replaceAll("<MachineSerialNumber>.*</MachineSerialNumber>", "<MachineSerialNumber>DeviceSerialNumber_152</MachineSerialNumber>")
        (0 to 20).foreach(_ => println())
        MachineLog.construct(text, -1)
      } catch {
        case _: Throwable => None
      }
    })

    machLogList.map(ml => makeMaintenanceRecordList(ml, -1, -1))
    println("\ndir: " + dir.getAbsolutePath)
  }
}
