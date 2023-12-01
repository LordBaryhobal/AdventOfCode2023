package day1

import scala.io.{BufferedSource, Source}

object Puzzle2 {
  val DIGITS: Array[String] = Array(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  )
  def loadInput(path: String): Int = {
    var sum: Int = 0
    val source: BufferedSource = Source.fromFile(path)
    for (line: String <- source.getLines()) {
      var a: Int = -1
      var b: Int = -1
      var i: Int = 0
      while (i < line.length) {
        val c: Char = line(i)
        var value: Int = -1
        if ('0' <= c && c <= '9') {
          value = c - '0'
        } else {
          for (j: Int <- DIGITS.indices) {
            val digit: String = DIGITS(j)
            if (value == -1 && i < line.length - digit.length + 1) {
              if (line.substring(i, i+digit.length) == digit) {
                value = j+1
                if (a == -1) {
                  i += digit.length - 1
                }
              }
            }
          }
        }
        if (value != -1) {
          if (a == -1) {
            a = value
          }
          b = value
        }

        i += 1
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
