import edu.umro.ImageUtil.ImageText
import edu.umro.ImageUtil.ImageUtil
import edu.umro.ScalaUtil.Trace
import org.aqa.webrun.winLutz360.WLRotator
import org.aqa.AQALine

import java.awt.image.BufferedImage
import java.awt.Color
import javax.vecmath.Point2d


/**
 * Program to create an image that illustrates how rotating the collimator rotates the X1 X2 Y1 Y2 edges.
 */
object X1X2Y1Y2RotatedEdgeImageMaker {

  private def drawSquare(bufImg: BufferedImage, collAngle: Double): Unit = {

    val size_pix = bufImg.getWidth / 2

    val top = (bufImg.getHeight / 2) - (size_pix / 2)
    val bot = (bufImg.getHeight / 2) + (size_pix / 2)

    val lft = (bufImg.getWidth / 2) - (size_pix / 2)
    val rgt = (bufImg.getWidth / 2) + (size_pix / 2)

    val center = new Point2d(bufImg.getWidth / 2, bufImg.getHeight / 2)

    def rot(pt: Point2d) = WLRotator.rotatePoint(pt, center, 360 - collAngle)

    val topLft = rot(new Point2d(top, lft))
    val topRgt = rot(new Point2d(top, rgt))
    val botLft = rot(new Point2d(bot, lft))
    val botRgt = rot(new Point2d(bot, rgt))

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)
    def drawLine(p1: Point2d, p2: Point2d): Unit = {
      gc.drawLine(p1.x.toInt, p1.y.toInt, p2.x.toInt, p2.y.toInt)
    }

    drawLine(topLft, topRgt)
    drawLine(topLft, botLft)
    drawLine(botLft, botRgt)
    drawLine(topRgt, botRgt)
  }

  private def labelEdges(bufImg: BufferedImage, collAngle: Double): Unit = {
    val center_pix = new Point2d(bufImg.getWidth / 2, bufImg.getHeight / 2)
    val xLine = AQALine(center_pix, collAngle)
    val yLine = xLine.perpendicular

    val radius = 20.0

    // case class Label(name: String, )

    val a = collAngle

    val xSign = {
      if (collAngle == 0) -1
      else if (collAngle <= 90) 1
      else if (collAngle < 135) -1
      else if (collAngle <= 225) 1
      else if (collAngle < 315) -1
      else 1
    }

    val ySign = {
      if (collAngle == 0) -1
      else if (collAngle < 45) 1
      else if (collAngle <= 135) -1
      else if (collAngle < 225) 1
      else if (collAngle < 270) -1
      else if (collAngle == 270) 1
      else if (collAngle <= 315) -1
      else 1
    }

    val x1Point = xLine.pointOn(radius * xSign)
    val x2Point = xLine.pointOn(radius * -xSign)
    val y1Point = yLine.pointOn(radius * -ySign)
    val y2Point = yLine.pointOn(radius * ySign)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)

    def lab(p: Point2d, name: String): Unit = {
      ImageText.drawTextCenteredAt(gc, p.getX, p.getY, name)
    }

    lab(x1Point, "X1")
    lab(x2Point, "X2")
    lab(y1Point, "Y1")
    lab(y2Point, "Y2")
  }

  private def doIt(collAngle: Double): BufferedImage = {

    val iSize = 100

    val rgb =
      ((((collAngle.round * 3) % 128) << 16) + //
        (((collAngle.round * 5) % 128) << 8) + //
        ((collAngle.round * 12) % 128)) //
      .toInt

    val bufImg = new BufferedImage(iSize, iSize, BufferedImage.TYPE_INT_RGB)
    (0 until iSize).foreach( //
      x => //
        { //
          (0 until iSize).foreach(y => //
            bufImg.setRGB(x, y, rgb)
          )
        }
    )

    drawSquare(bufImg, collAngle)

    val gc = ImageUtil.getGraphics(bufImg)
    gc.setColor(Color.white)
    ImageText.drawTextCenteredAt(gc, 20, 20, collAngle.round.toString)

    labelEdges(bufImg, collAngle)

    bufImg
  }

  def main(args: Array[String]): Unit = {

    val list = (0 until 360 by 5).map(collAngle => doIt(collAngle))

    val bigImg = new BufferedImage(list.head.getWidth * 9, list.head.getHeight * 8, BufferedImage.TYPE_INT_RGB)

    def putInBigImg(i: Int): Unit = {
      val bigX = (i % 9) * list.head.getWidth
      val bigY = (i / 9) * list.head.getHeight

      (0 until list.head.getWidth).foreach( //
        x => { //
          (0 until list.head.getHeight).foreach( //
            y => //
              bigImg.setRGB(bigX + x, bigY + y, list(i).getRGB(x, y))
          )
        }
      )
    }

    list.indices.foreach(putInBigImg)

    Trace.showInMSPaint(bigImg)

    Thread.sleep(2000)

  }

}
