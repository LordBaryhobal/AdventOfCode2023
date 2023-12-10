package day10

import scala.collection.immutable.HashMap
import scala.io.{BufferedSource, Source}

object Puzzle1 {
  var grid: Array[Array[Byte]] = Array.empty

  // bits: NWSE
  val TILES: Map[Char, Byte] = HashMap(
    '|' -> 10,
    '-' -> 5,
    'L' -> 9,
    'J' -> 12,
    '7' -> 6,
    'F' -> 3,
    '.' -> 0,
    'S' -> 15
  )

  var height: Int = 0
  var width: Int = 0
  var startX: Int = 0
  var startY: Int = 0

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    height = lines.length
    width = if (height == 0) 0 else lines(0).length
    grid = Array.ofDim(height, width)

    for ((line: String, y: Int) <- lines.zipWithIndex) {
      for ((c: Char, x: Int) <- line.zipWithIndex) {
        grid(y)(x) = TILES(c)
        if (c == 'S') {
          startX = x
          startY = y
        }
      }
    }

    source.close()
  }

  def calculateMaxDistance(): Int = {
    val walker1: Walker = new Walker(startX, startY)
    walker1.walk(grid)
    val walker2: Walker = new Walker(startX, startY, walker1.getX(), walker1.getY())

    while (walker1.getPos() != walker2.getPos()) {
      walker2.walk(grid)
      if (walker1.getPos() == walker2.getPos()) {
        return walker2.getDistance()
      }
      walker1.walk(grid)
    }
    return walker1.getDistance()
  }

  def solve(path: String): Int = {
    loadInput(path)

    val solution: Int = calculateMaxDistance()

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day10/input1.txt")
    println(solution)
  }
}
