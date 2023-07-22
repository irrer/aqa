package org.aqa.stats

object Txt {
  private val set = scala.collection.mutable.HashSet[String]()

  def get(txt: String): String = {
    txt
    /*
    if (!set.contains(txt))
      set += new String(txt)
    set.find(t => t.equals(txt)).get
    */
  }
}
