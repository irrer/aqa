package org.aqa

object AQATypes {
  type ImSeq[T] = scala.collection.immutable.IndexedSeq[T]

  def toImSeq[T](seq: Iterable[T]): ImSeq[T] = {
    val imSeq: ImSeq[T] = scala.collection.immutable.IndexedSeq() ++ seq
    imSeq
  }

}

