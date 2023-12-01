package day1

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  def loadInput(path: String): Int = {
    var sum: Int = 0
    val source: BufferedSource = Source.fromFile(path)
    for (line: String <- source.getLines()) {
      var a: Int = -1
      var b: Int = -1
      for (c: Char <- line) {
        if ('0' <= c && c <= '9') {
          if (a == -1) {
            a = c - '0'
          }
          b = c - '0'
        }
      }
      sum += a*10 + b
    }
    source.close()
    return sum
  }
  def main(args: Array[String]): Unit = {
    val sum: Int = loadInput("res/day1/input1.txt")
    println(s"sum = $sum")
  }
}
