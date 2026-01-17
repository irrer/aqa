package learn

import scala.util.Random

/**
  * This demonstrates why you should use def instead of val in a trait.
 *
 * It has to do with how objects are initialized:
 *
 * Lesson is that the area and cost values should be def's, not val's.
 *
 * From GPT-4.1:
 * But in traits, Scala initializes members in the order they appear, before the subclass constructor has run.
 * When instantiating Traitor, the Area trait's area and cost values are initialized before Traitor's width and height are available!
  */

trait Area {
  val width: Double
  val height: Double
  val area: Double = width * height // def works
  val cost: Double = area * area // def works
}

case class Traitor(width: Double, height: Double) extends Area {}

object Traitor {

  var failureCount = 0
  val attempts = 5000

  def main(args: Array[String]): Unit = {

    val rand = new Random

    def nextDbl(): Double = {
      Thread.sleep(20)
      rand.nextDouble()
    }

    (0 until attempts).toList.par.foreach(_ => {
      //  (0 until 50).toList.par.foreach(_ => {
      val a = Traitor(nextDbl(), nextDbl())
      val fD = (a.width * a.height) * (a.width * a.height)
      if (a.cost != fD) {
        println(s"badness cost!\n    is       : ${a.cost}\n    should be: $fD")
        failureCount = failureCount + 1
      }
    })

    println(s"Done. Attempts: $attempts    Failures: $failureCount")
  }
}
