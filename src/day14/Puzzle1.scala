package day14

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  val CHARS: Array[Char] = Array('.', 'O', '#')
  var platform: Array[Array[Int]] = Array.empty
  var width: Int = 0
  var height: Int = 0

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    platform = lines.map(_.map(CHARS.indexOf(_)).toArray)
    height = platform.length
    width = if (height == 0) 0 else platform(0).length

    source.close()
  }

  def slideUp(): Unit = {
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (platform(y)(x) == 1) {
          slideRockUp(x, y)
        }
      }
    }
  }

  def slideRockUp(x: Int, y: Int): Unit = {
    platform(y)(x) = 0
    for (y2: Int <- y-1 to 0 by -1) {
      if (platform(y2)(x) != 0) {
        platform(y2+1)(x) = 1
        return
      }
    }
    platform(0)(x) = 1
  }

  def computeLoad(): Int = {
    var load: Int = 0
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (platform(y)(x) == 1) {
          load += height-y
        }
      }
    }
    return load
  }

  def solve(path: String): Int = {
    loadInput(path)
    slideUp()

    val solution: Int = computeLoad()

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day14/input1.txt")
    println(solution)
  }
}
