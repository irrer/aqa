package org.aqa.webrun.wl.wlMonthly

import org.aqa.webrun.ExtendedData
import org.aqa.Logging

case class WLMonthly(
                      // @formatter:off
                      extendedData: ExtendedData,
                      G180C000T000: WLBeam, G180C090T000: WLBeam,
                      G090C090T000: WLBeam, G000C090T000: WLBeam,
                      G270C090T000: WLBeam, G270C270T000: WLBeam,
                      G000C270T000: WLBeam, G090C270T000: WLBeam, G180C270T000: WLBeam,
                      G180C270T270: WLBeam, G180C270T300: WLBeam
                      // @formatter:on
                    ) extends Logging {
}

object WLMonthly extends Logging {

  /**
   * Determine if all the data is present to construct a WL Monthly data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param pairList     List of incoming DICOM and results.
   * @return Monthly data set or None.
   */
  def make(extendedData: ExtendedData, pairList: Seq[WLPairDbAl] ): Option[WLMonthly] = {

    val machine = extendedData.machine

    def findPair(g: Int, c: Int, t: Int): Option[WLBeam] = {
      WLPairDbAl.findGCT(pairList, g, c, t).map(pair => WLBeam(pair.wl, pair.al, machine))
    }

    // @formatter:off
    val G180C000T000: Option[WLBeam] = findPair(180,   0,   0)
    val G180C090T000: Option[WLBeam] = findPair(180,  90,   0)

    val G090C090T000: Option[WLBeam] = findPair( 90,  90,   0)
    val G000C090T000: Option[WLBeam] = findPair(  0,  90,   0)

    val G270C090T000: Option[WLBeam] = findPair(270,  90,   0)
    val G270C270T000: Option[WLBeam] = findPair(270, 270,   0)

    val G000C270T000: Option[WLBeam] = findPair(  0, 270,   0)
    val G090C270T000: Option[WLBeam] = findPair( 90, 270,   0)
    val G180C270T000: Option[WLBeam] = findPair(180, 270,   0)

    val G180C270T270: Option[WLBeam] = findPair(180, 270, 270)
    val G180C270T300: Option[WLBeam] = findPair(180, 270, 300)
    // @formatter:on

    // list of all files required for WL Monthly
    val list = Seq(
      // @formatter:off
      G180C000T000, G180C090T000,
      G090C090T000, G000C090T000,
      G270C090T000, G270C270T000,
      G000C270T000, G090C270T000, G180C270T000,
      G180C270T270, G180C270T300,
      // @formatter:on
    )

    // if of the files are there then construct the object, otherwise return None.
    if (list.flatten.size == list.size) {
      Some(WLMonthly(
        extendedData,
        // @formatter:off
        G180C000T000.get, G180C090T000.get,
        G090C090T000.get, G000C090T000.get,
        G270C090T000.get, G270C270T000.get,
        G000C270T000.get, G090C270T000.get, G180C270T000.get,
        G180C270T270.get, G180C270T300.get
        // @formatter:on
      ))
    }
    else
      None
  }
}
