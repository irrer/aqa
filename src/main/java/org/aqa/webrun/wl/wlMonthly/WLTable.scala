package org.aqa.webrun.wl.wlMonthly

import org.aqa.webrun.ExtendedData

case class WLTable(
                    // @formatter:off
                    extendedData: ExtendedData,
                    T000: WLBeam,
                    T030: WLBeam,
                    T060: WLBeam,
                    T090: WLBeam,
                    T270: WLBeam,
                    T300: WLBeam,
                    T330: WLBeam
                    // @formatter:on
                  ) {


}

object WLTable {

  /**
   * Determine if all the data is present to construct a WL Table data set.  If so, make one and return it.
   *
   * @param extendedData Metadata.
   * @param pairList     List of incoming DICOM and results.
   * @return Table data set or None.
   */
  def make(extendedData: ExtendedData, pairList: Seq[WLBeam]): Option[WLTable] = {

    val machine = extendedData.machine

    def findPair(g: Int, c: Int, t: Int): Option[WLBeam] = {
      WLBeam.findGCT(pairList, g, c, t)
    }


    // @formatter:off
    val T000 : Option[WLBeam] = findPair( 180, 270,   0 )
    val T030 : Option[WLBeam] = findPair( 180, 270,  30 )
    val T060 : Option[WLBeam] = findPair( 180, 270,  60 )
    val T090 : Option[WLBeam] = findPair( 180, 270,  90 )
    val T270 : Option[WLBeam] = findPair( 180, 270, 270 )
    val T300 : Option[WLBeam] = findPair( 180, 270, 300 )
    val T330 : Option[WLBeam] = findPair( 180, 270, 330 )
    // @formatter:on

    // list of all files required for WL Table
    val list = Seq(
      T000,
      T030,
      T060,
      T090,
      T270,
      T300,
      T330
    )

    // if of the files are there then construct the object, otherwise return None.
    if (list.flatten.size == list.size) {
      Some(WLTable(extendedData,
        T000.get,
        T030.get,
        T060.get,
        T090.get,
        T270.get,
        T300.get,
        T330.get
      ))
    }
    else
      None
  }
}
