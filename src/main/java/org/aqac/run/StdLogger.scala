package org.aqac.run

import org.aqac.db.Output
import scala.sys.process._
import java.io.FileWriter
import java.io.Closeable
import java.io.Flushable
import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

/**
 * Write stdout and stderr to a single file, prefixing each line with 'out' or 'err' and a time stamp.
 */

class StdLogger(output: Output) extends ProcessLogger with Closeable with Flushable {
    private val LS = System.lineSeparator
    private val textFile = new File(output.dir, StdLogger.LOG_TEXT_FILE_NAME)
    private val outputStream = new FileWriter(textFile, true)
    private def timestamp: String = StdLogger.logTimestampFormat.format(new Date(System.currentTimeMillis))
    private def writeString(prefix: String, msg: String): Unit = {
        outputStream.synchronized({
            val newline = if (msg.endsWith(LS) || msg.endsWith("\n")) "" else LS
            val text = prefix + timestamp + " " + msg + newline
            outputStream.write(text)
            outputStream.flush()
        })
    }

    override def out(s: => String): Unit = writeString("out ", s)
    override def err(s: => String): Unit = writeString("err ", s)
    override def buffer[T](f: => T): T = f
    override def close(): Unit = outputStream.close()
    override def flush(): Unit = outputStream.flush()
}

object StdLogger {

    val LOG_TEXT_FILE_NAME = "log.txt"

    val logTimestampFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS")

}