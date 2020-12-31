package learn

import com.typesafe.config.ConfigFactory
import edu.umro.ScalaUtil.Trace
import java.io.File

object SimpleLibContextMin {
  def main(args: Array[String]): Unit = {
    Trace.trace("Starting")
    val file = new File("reference.conf")
    val config = ConfigFactory.parseFile(file)
    val foo: String = config.getString("simple-lib.foo")
    Trace.trace("foo: " + foo)
    //val stuff = com.typesafe.config.ConfigFactory.load(config)
    //Trace.trace(stuff.toString.split(",").mkString("\n"))
    Trace.trace("Done")
  }
}


