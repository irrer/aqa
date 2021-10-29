package learn

import java.io.File
import scala.xml.Node
import scala.xml.XML

/**
 * Exploratory code for processing machine logs.
 */
object DumpMachLog {

  /**
    * Describe a list of nodes within a machine log.
    * @param node x
    * @param groupList x
    */
  private case class LogGroup(node: Node, groupList: Seq[Node]) {

    private def nameOf(n: Node) = (n \ "@name").text

    override def toString: String = {
      nameOf(node) + " :: " + groupList.map(nameOf).mkString(" / ")
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

  private def showFile(file: File): Unit = {
    val doc = XML.loadFile(file)
    val nodeList = doc \ "Node"
    println(file.getName)
    nodeList.flatMap(findGroups).foreach(g => println("    " + g))
    println
  }

  def main(args: Array[String]): Unit = {

    println("Starting ...")
    val start = System.currentTimeMillis()

    if (false) {
      val file = new File("""D:\tmp\aqa\MachineLogs\H192448\H192448\SavedConfigParameters_2015-11-25_05-48-12_AM.xml""")
      showFile(file)
    }

    if (false) {
      val file = new File("""D:\tmp\aqa\MachineLogs\H192448\H192448\SavedConfigParameters_2015-11-25_05-48-12_AM.xml""")
      val n = XML.loadFile(file)
      (n \ "Node").foreach(n => println(findGroups(n).mkString("\n")))
    }

    println("Done.  Elapsed ms: " + (System.currentTimeMillis() - start))
  }
}
