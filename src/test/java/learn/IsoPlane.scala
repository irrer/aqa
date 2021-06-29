/*
 * Copyright 2021 Regents of the University of Michigan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package learn

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{ Failure, Success }
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import java.util.concurrent.TimeUnit
import edu.umro.ScalaUtil.Trace
import akka.actor._
import edu.umro.ImageUtil.IsoImagePlaneTranslator
import com.pixelmed.dicom.AttributeList

object IsoPlane {

  val start = System.currentTimeMillis
  def elapsed: String = (System.currentTimeMillis - start).formatted("%8d    ")

  def main(args: Array[String]): Unit = {

    Trace.trace

    val al = new AttributeList
    al.read("""D:\pf\eclipse\workspaceOxygen\ImageUtil\src\test\resources\TestLocateEdge\abstractNoBall.dcm""")
    val isoTrans = new IsoImagePlaneTranslator(al)
    val pt = isoTrans.pix2Iso(600.5, 639.5)
    println("pt: " + pt)
    Trace.trace
    println(elapsed + "exiting with System.exit")
    System.exit(0)
  }

}
