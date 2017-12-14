package test

//import org.slf4j.Logger
//import org.slf4j.LoggerFactory

//import play.api.Logger

object LogMe {

    def logme(any: Any) = {
        val se: StackTraceElement = Thread.currentThread.getStackTrace()(3)
        
        
        //   se.get
        val clazz = se.getClassName
        org.slf4j.LoggerFactory.getLogger("").info(any.toString)
    }

}