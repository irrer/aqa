package org.aqa.webrun.wl.wlMonthly

import org.aqa.webrun.ExtendedData

case class WLTable(
                    // @formatter:off
                 extendedData: ExtendedData,
                 T300: WLBeam,
                 T330: WLBeam,
                 T060: WLBeam,
                 T090: WLBeam,
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
  def make(extendedData: ExtendedData, pairList: Seq[WLPairDbAl]): Option[WLTable] = {

    val machine = extendedData.machine

    def findPair(g: Int, c: Int, t: Int): Option[WLBeam] = {
      WLPairDbAl.findGCT(pairList, g, c, t).map(pair => WLBeam(pair.wl, pair.al, machine))
    }


    // @formatter:off
      val T330 : Option[WLBeam] = findPair( 180, 270, 330 )
      val T030 : Option[WLBeam] = findPair( 180, 270,  30 )
      val T060 : Option[WLBeam] = findPair( 180, 270,  60 )
      val T090 : Option[WLBeam] = findPair( 180, 270,  90 )
      // @formatter:on

    // list of all files required for WL Table
    val list = Seq(T330, T030, T060, T090)

    // if of the files are there then construct the object, otherwise return None.
    if (list.flatten.size == list.size) {
      Some(WLTable(extendedData, T330.get, T030.get, T060.get, T090.get))
    }
    else
      None
  }
}
