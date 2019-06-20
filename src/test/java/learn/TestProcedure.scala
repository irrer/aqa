package learn

object TestProcedure {
    def main(args: Array[String]): Unit = {
        println("starting")
        Thread.sleep(100)
        println("this is for stdout")
        Thread.sleep(100)
        Console.err.println("this is for stderr")
        Thread.sleep(100)
        println("finished")
    }

}