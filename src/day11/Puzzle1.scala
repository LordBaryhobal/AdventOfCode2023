package day11

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle1 {
  var ogUniverse: Array[Array[Boolean]] = Array.empty
  var ogHeight: Int = 0
  var ogWidth: Int = 0
  var colCounts: Array[Int] = Array.empty
  var rowCounts: Array[Int] = Array.empty
  var galaxies: ArrayBuffer[(Int, Int)] = new ArrayBuffer()
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    ogHeight = lines.length
    ogWidth = if (ogHeight == 0) 0 else lines(0).length
    ogUniverse = Array.ofDim(ogHeight, ogWidth)

    colCounts = new Array(ogWidth)
    rowCounts = new Array(ogHeight)

    for ((line: String, y: Int) <- lines.zipWithIndex) {
      for ((c: Char, x: Int) <- line.zipWithIndex) {
        if (c == '#') {
          ogUniverse(y)(x) = true
          colCounts(x) += 1
          rowCounts(y) += 1
        }
      }
    }

    source.close()
  }

  def findGalaxiesRealPos(): Unit = {
    galaxies = new ArrayBuffer()

    var realX: Int = 0
    var realY: Int = 0

    for (ogY: Int <- 0 until ogHeight) {
      realX = 0
      for (ogX: Int <- 0 until ogWidth) {
        if (ogUniverse(ogY)(ogX)) {
          galaxies.addOne((realX, realY))
        }
        if (colCounts(ogX) == 0) realX += 1
        realX += 1
      }
      if (rowCounts(ogY) == 0) realY += 1
      realY += 1
    }
  }

  def distance(g1: (Int, Int), g2: (Int, Int)): Int = {
    return Math.abs(g2._1 - g1._1) + Math.abs(g2._2 - g1._2)
  }

  def solve(path: String): Int = {
    loadInput(path)
    findGalaxiesRealPos()

    var solution: Int = 0

    for (i: Int <- galaxies.indices) {
      val g1: (Int, Int) = galaxies(i)
      for (j: Int <- i+1 until galaxies.length) {
        val g2: (Int, Int) = galaxies(j)
        solution += distance(g1, g2)
      }
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day11/input1.txt")
    println(solution)
  }
}
