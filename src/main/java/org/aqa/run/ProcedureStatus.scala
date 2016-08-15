package org.aqa.run

//import collection.JavaConverters._
import java.io.FileInputStream
import java.io.File
import resource.managed
import scala.collection.mutable.ArrayBuffer
import java.io.FileOutputStream

private object ProcedureStatusList {
    private val statusList = ArrayBuffer[ProcedureStatus.ProcedureStatus]()

    def add(status: ProcedureStatus.ProcedureStatus) = statusList += status

    def list = statusList.toList
}

/**
 * Various types of procedure termination statuses.  Add more as needed.
 */
object ProcedureStatus extends Enumeration {

    /** When storing a ProcedureStatus in a file, the file must have this name. */
    val statusFileName = "status.txt"

    val running = Value("running", "Procedure is currently running.")
    val pass = Value("pass", "All results are within acceptable limits.")
    val fail = Value("fail", "At least one result was outside acceptable limits.")
    val done = Value("done", "Completed, but no indication as to whether results are acceptable or not.")
    val abort = Value("abort", "Prematurely terminated itself.")
    val dberr = Value("dberr", "Problem using the database from the procedure.")
    val userabort = Value("userabort", "Prematurely terminated by user.")
    val timeout = Value("timeout", "Terminated when time limit and was exceeded.")
    val crash = Value("crash", "Terminated itself in an uncontrolled fashion.")

    class ProcedureStatus(val name: String, val description: String) extends Val(nextId, name)

    protected final def Value(name: String, description: String): ProcedureStatus = {
        val status = new ProcedureStatus(name, description)
        ProcedureStatusList.add(status)
        status
    }

    private val maxNameLength = ProcedureStatus.values.map(s => s.toString.size).max

    def stringToProcedureStatus(name: String): Option[ProcedureStatus.Value] = {
        val matches = ProcedureStatus.values.filter(s => name.equalsIgnoreCase(s.toString)).toList
        if (matches.isEmpty) None else Some(matches.head)
    }

    def descriptionOf(status: ProcedureStatus.Value): String = {
        val name = status.toString
        ProcedureStatusList.list.filter(s => s.name.equals(name)).head.description
    }

    /**
     * Given a file, try to read a status (by name) from it.
     */
    def fileToProcedureStatus(file: File): Option[ProcedureStatus.Value] = {
        try {
            val buf = Array.ofDim[Byte](maxNameLength)
            managed(new FileInputStream(file)) acquireAndGet {
                fis =>
                    {
                        val size = fis.read(buf)
                        val name = new String(buf).substring(0, size).toLowerCase.trim
                        stringToProcedureStatus(name)
                    }
            }
        }
        catch {
            case t: Throwable => None
        }
    }
    
    def dirToProcedureStatus(dir: File): Option[ProcedureStatus.Value] = ProcedureStatus.fileToProcedureStatus(new File(dir, ProcedureStatus.statusFileName))

    def writeProcedureStatus(dir: File, status: ProcedureStatus.Value): Unit = {
        try {
            val statusFile = new File(dir, statusFileName)
            managed(new FileOutputStream(statusFile)) acquireAndGet {
                fos =>
                    {
                        fos.write(status.toString.getBytes)
                    }
            }
        }
        catch {
            case t: Throwable => ;
        }
    }

    /**
     * Sort by id.
     */
    def sort(seq: Seq[ProcedureStatus.Value]) = seq.sortWith((a, b) => a.id < b.id)

    /**
     * For internal testing only.
     */
    def main(args: Array[String]): Unit = {

        val first = ProcedureStatus.values.toArray.toList(0)

        if (true) {
            println("starting")
            val s = stringToProcedureStatus("timeout")
            println("s: " + s)
            System.exit(99)
        }

        if (true) {
            println("cwd: " + (new File(".")).getAbsolutePath)
            println("status.good: " + fileToProcedureStatus(new File("status.good")))
            println("status.bad: " + fileToProcedureStatus(new File("status.bad")))
            System.exit(99)
        }

        if (true) {
            val lst = List(abort, fail, done, pass)
            println("lst: " + lst)
            val srt = sort(lst)
            println("srt: " + srt)
            System.exit(99)
        }

        println("first: " + first)

        def show(s: ProcedureStatus.ProcedureStatus) = {
            println
            println("s: " + s)
            println("s.toString: " + s.toString)
            println("s.description: " + s.description)
            println("s.asInstanceOf[ProcedureStatus.ProcedureStatus].description: " + s.asInstanceOf[ProcedureStatus.ProcedureStatus].description)
            println("s.+(\"hey\"): " + s.+("hey"))
            println("s.id: " + s.id)
        }
        println
        println("ProcedureStatus.maxId: " + ProcedureStatus.maxId)

        val p = ProcedureStatus.pass
        show(p)
        show(ProcedureStatus.abort)

        val newProcedureStatus = ProcedureStatus.Value("hey", "Well hello there")
        show(newProcedureStatus)

        println
        println("p.description: " + p.description)

        /*
        println
        println("ProcedureStatus.construct(\"crash\"): " + ProcedureStatus.construct("crash"))
        println("ProcedureStatus.construct(\"baddy\"): " + ProcedureStatus.construct("baddy"))
        println("ProcedureStatus.construct(\"Done\"): " + ProcedureStatus.construct("Done"))
        */
    }
}
