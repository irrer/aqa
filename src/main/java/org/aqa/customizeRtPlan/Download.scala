package org.aqa.customizeRtPlan

import java.io.File
import scala.xml.Elem

/**
 * Information required to display a file for download.  The file may either be DICOM or a zip file.
 * @param elem Shows file to user.
 * @param file File available to user for download.
 */
case class Download(elem: Elem, file: File) {}
