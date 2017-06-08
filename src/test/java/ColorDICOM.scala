
import java.io.File
import com.pixelmed.dicom.AttributeList
import com.pixelmed.dicom.TagFromName
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

object ColorDICOM {

    def getRawPixels(attributeList: AttributeList): Array[Array[Int]] = {
        val height: Int = attributeList.get(TagFromName.Rows).getIntegerValues()(0)
        val width: Int = attributeList.get(TagFromName.Columns).getIntegerValues()(0)
        val shorts: Array[Short] = attributeList.get(TagFromName.PixelData).getShortValues
        JavaUtil.pixelDataToArray(height, width, shorts)
    }

    def p2ColorJI(pixel: Float): Int = {
        val range = 0x1ff
        val p = (pixel * range).floor.toInt
        val half = range / 2

        val red = (p >> 1) & 0xff
        val green = (if (pixel < half) (p & 0xff) else (256 - (p >> 8))) & 0xff
        val blue = (0xff - (p & 0xff)) & 0xff
        (red << 16) + (green << 8) + blue
    }

    def p2BW(pixel: Float): Int = {
        val p = ((pixel * 0xff).floor.toInt) & 0xff
        (p << 16) + (p << 8) + p
    }

    def p2ColorLR(pixel: Float): Int = {
        val f = pixel
        val a = (1 - f) / 0.2
        val X = Math.floor(a)
        val Y = Math.floor(255 * (a - X))

        var r: Double = 0
        var g: Double = 0
        var b: Double = 0

        X match {
            case 0 => { r = 255; g = Y; b = 0; }
            case 1 => { r = 255 - Y; g = 255; b = 0; }
            case 2 => { r = 0; g = 255; b = Y; }
            case 3 => { r = 0; g = 255 - Y; b = 255; }
            case 4 => { r = Y; g = 0; b = 255; }
            case 5 => { r = 255; g = 0; b = 255; }
        }
        (r.toInt << 16) + (g.toInt << 8) + b.toInt
    }

    def write(al: AttributeList, dest: File): Unit = {
        val height: Int = al.get(TagFromName.Rows).getIntegerValues()(0)
        val width: Int = al.get(TagFromName.Columns).getIntegerValues()(0)

        val pix = getRawPixels(al)

        val pixFlat = pix.flatten

        val min = pixFlat.min.toFloat
        val max = pixFlat.max.toFloat
        val range = max - min

        println("min: " + min + "    max: " + max + "   " + dest.getName)

        val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
        for (row <- (0 until height); col <- (0 until width)) {
            val p = if (range == 0) 0 else { ((pix(row)(col)) - min) / range }
            //image.setRGB(col, row, p2BW(p))
            image.setRGB(col, row, p2ColorLR(p))
        }
        dest.delete
        ImageIO.write(image, "png", dest)
    }

    def write(dicomFile: File): Unit = {
        //val fileName = dicomFile.getName.toLowerCase.replace(".dcm", "bw.png")
        val fileName = dicomFile.getName.toLowerCase.replace(".dcm", "lr.png")
        val pngFile = new File(dicomFile.getParentFile, fileName)

        val al = new AttributeList
        al.read(dicomFile)
        write(al, pngFile)
    }

    def main(args: Array[String]): Unit = {
        println("starting")

        //        val rng = 256 * 256 * 16
        //        val p2ColorLRMap = (0 to rng).map(p => p2ColorLR((p.toFloat) / rng)).distinct
        //        println("tried: " + rng + "    num: " + p2ColorLRMap.size)

        // val dir = new File("""D:\tmp\ColorDICOM""")
        // val dir = new File("""D:\tmp\wl\output""")
        val dir = new File("""D:\tmp\wl\ct2\QASRSWLCTCT2\output""")

        // val file = new File("""D:\AQA_Data\results\Chicago_33\TB5x_1\Leaf_Correction_and_Transmission_1.0_5\2017-03-03T10-13-50-817_170\RI.1.2.246.352.62.1.5007788524908277936.11459800010619178630.dcm""")
        // val file = new File("""D:\AQA_Data\results\TBD_2\CHIC2_12\Upload_TRANS_and_OPEN_base_DICOM_files_1.0.0_1\2017-04-04T14-44-28-279_52\OPEN_BaselineCHIC2.dcm""")
        // val file = new File("""D:\pf\Conquest\dicomserver1419beta3b\data\000000066\1.2.840.113704.1.111.6492.1435009699.6_0002_000051_144181911500a2.dcm""")
        // val file = new File("""D:\pf\Conquest\dicomserver1419beta3b\data\28331927\1.2.840.113704.1.111.8116.1460994927.16_0002_000065_146100587206a3.dcm""")

        dir.listFiles.toSeq.filter(f => f.getName.toLowerCase.endsWith(".dcm")).map(f => write(f))

        println("finished")
    }

}