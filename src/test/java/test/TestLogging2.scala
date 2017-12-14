package test

trait LogIt {
    val logger = org.slf4j.LoggerFactory.getLogger("")
    implicit def logit(any: Any) = {
        val msg = any.toString
        logger.info(msg)
    }
}

private class TestLogging2 extends LogIt {
    import LogMe._

    val logIt: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger("")
    val hi = "hi from TestLogging2"
    logIt.info(hi)
    logit(hi)

    def func = {
        val msg = "from logme from func"
        logger.info(msg)
        //        logger.info(???, "")
        logIt.info("logIt in func")
    }

    class Inside {
        logger.info("Inside")
        logIt.info("logIt in Inside")
        def funcy = {
            logIt.info("logIt funcy in Inside")
        }
        funcy
    }

    class Inside2 extends LogIt {
        logger.info("Inside2")
        logIt.info("logIt in Inside2")
        def funcy = {
            logIt.info("logIt funcy in Inside2")
        }
        funcy
    }

    func

    (new Inside).funcy
    (new Inside2).funcy

    println("exiting PlayClass")
}

object Thingy extends LogIt {
    def dolog = {
        logger.info("dolog is talking")
    }
}

object TestLogging2 {
    def main(args: Array[String]): Unit = {
        println("starting")
        (new TestLogging2)
        Thingy.dolog
    }

}
