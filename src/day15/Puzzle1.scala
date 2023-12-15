package day15

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  var values: Array[String] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val line: String = source.getLines().mkString

    values = line.split(",")

    source.close()
  }

  def hash(str: String): Int = {
    var value: Int = 0
    for (c: Char <- str) {
      value += c
      value *= 17
      value %= 256
    }
    return value
  }

  def solve(path: String): Int = {
    loadInput(path)

    var solution: Int = 0

    for (value: String <- values) {
      solution += hash(value)
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day15/input1.txt")
    println(solution)
  }
}
