package day9

import scala.collection.mutable.ArrayBuffer

class Series {
  private val values: ArrayBuffer[Array[Int]] = new ArrayBuffer()
  def this(initialValues: Array[Int]) = {
    this()
    values.addOne(initialValues.concat(Array(0)))
  }

  def computeDiffs(): Unit = {
    while (!(values.last.dropRight(1).distinct.length == 1 && values.last(0) == 0)) {
      val diffs: Array[Int] = new Array(values.last.length-1)
      for (i: Int <- 0 until diffs.length-1) {
        diffs(i) = values.last(i+1) - values.last(i)
      }
      values.addOne(diffs)
    }
  }

  def extrapolate(): Int = {
    val len: Int = values(0).length
    values.last(values.last.length-1) = 0
    for (i: Int <- values.length-2 to 0 by -1) {
      values(i)(len-i-1) = values(i)(len-i-2) + values(i+1)(len-i-2)
    }
    return values(0).last
  }

  def reverseExtrapolate(): Int = {
    val len: Int = values(0).length
    values.last(values.last.length-1) = 0
    for (i: Int <- values.length-2 to 0 by -1) {
      values(i)(len-i-1) = values(i)(0) - values(i+1)(len-i-2)
    }
    return values(0).last
  }
}
