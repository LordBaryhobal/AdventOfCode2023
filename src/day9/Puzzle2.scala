package day9

import scala.io.{BufferedSource, Source}

object Puzzle2 {
  var series: Array[Series] = Array.empty

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    series = new Array(lines.length)

    for ((line: String, i: Int) <- lines.zipWithIndex) {
      series(i) = new Series(line.split(" ").map(_.toInt))
    }

    source.close()
  }

  def solve(path: String): Int = {
    loadInput(path)

    var solution: Int = 0

    for (s: Series <- series) {
      s.computeDiffs()
      solution += s.reverseExtrapolate()
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day9/input1.txt")
    println(solution)
  }
}
