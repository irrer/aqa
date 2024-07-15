package org.aqa.webrun.seriesMaker

import com.pixelmed.dicom.AttributeList
import edu.umro.DicomDict.TagByName
import edu.umro.ScalaUtil.DicomUtil
import edu.umro.ScalaUtil.FileUtil
import edu.umro.ScalaUtil.FileUtil.ToZipOutputStream
import org.aqa.webrun.phase2.Phase2Util
import org.aqa.Logging

import java.io.ByteArrayOutputStream

/**
  * Maintain a cache of file assignments.  This allows the user to both download and run the exact same files.  The cache
  */
object SeriesCache extends Logging {

  /**
    * A set of files to save.
    * @param key Unique identifier.
    * @param content Bytes of files.
    */
  case class Entry(key: String, content: Array[Byte]) {

    /** When this entry was created. */
    val time: Long = System.currentTimeMillis()
  }

  private val cache = scala.collection.mutable.HashMap[String, Entry]()

  private val cacheSizeLimit = 10

  /**
    * Make a unique key for the given context.
    * @param session HTML session.
    * @param beamAssignmentList List of which images are assigned to which beams.
    * @return Unique key.
    */
  private def makeKey(session: String, beamAssignmentList: String) = s"$session => $beamAssignmentList"

  /**
    * Make a file name for the given RTIMAGE.  Use the beam name if possible, otherwise use the beam number.
    * @param rtimage RTIMAGE
    * @param rtplan RTPLAN
    * @return Name of RTIMAGE.
    */
  private def fileName(rtimage: AttributeList, rtplan: AttributeList): String = {

    val beamName: String = Phase2Util.getBeamNameOfRtimage(rtplan, rtimage) match {
      case Some(bn) => FileUtil.replaceInvalidFileNameCharacters(bn, '_')
      case _ =>
        val beamNumber = rtimage.get(TagByName.ReferencedBeamNumber).getIntegerValues.head
        beamNumber.toString
    }

    val name = s"RTIMAGE_$beamName.dcm"
    name
  }

  /**
    * Put the given beam assignments into a local cache.
    * @param session HTML session.
    * @param beamAssignmentList List of which images are assigned to which beams.
    * @param rtimageList List of RTIMAGES to be downloaded or run (not anonymized).
    * @param rtplan Plan
    * @return contents
    */
  def put(session: String, beamAssignmentList: String, rtimageList: Seq[AttributeList], rtplan: AttributeList): Entry = {
    val zip = new ToZipOutputStream

    /**
      * Write an RTIMAGE to the zip stream.
      * @param rtimage File to write.
      */
    def writeDicom(rtimage: AttributeList): Unit = {
      val out = new ByteArrayOutputStream()
      DicomUtil.writeAttributeList(rtimage, out, "AQA")
      zip.write(out.toByteArray, fileName(rtimage, rtplan))
    }

    rtimageList.foreach(writeDicom)

    val content = zip.finish()

    val key = makeKey(session, beamAssignmentList)

    val entry = Entry(key, content)

    SeriesCache.synchronized {
      cache.put(key, Entry(key, content))
      while (cache.size > cacheSizeLimit) {
        val oldest = cache.values.minBy(_.time)
        cache.remove(oldest.key)
        logger.info(s"Removed cache entry $key")
      }
    }

    logger.info(s"Put series to cache.  key: ${key}.take(36)   files: ${rtimageList.size} content size in bytes: ${content.length}")

    entry
  }

}
