package day3

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  val OFFSETS: Array[Array[Int]] = Array(
    Array(-1,-1), Array( 0,-1), Array( 1,-1),
    Array(-1, 0),               Array( 1, 0),
    Array(-1, 1), Array( 0, 1), Array( 1, 1)
  )
  var width = 0
  var height = 0

  var schematic: Array[Array[Char]] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray
    height = lines.length
    width = if (height == 0) 0 else lines(0).length
    schematic = new Array(height)

    var i = 0
    for (line: String <- lines) {
      schematic(i) = line.split("").map(s => s(0))
      i += 1
    }

    source.close()
  }

  def getNumber(sx: Int, sy: Int): String = {
    var x: Int = sx
    var number: String = ""
    var c: Char = schematic(sy)(sx)
    do {
      number += c
      x += 1
      if (x < width) c = schematic(sy)(x)
    } while ('0' <= c && c <= '9' && x < width)
    return number
  }

  def isPartNumber(sx: Int, sy: Int, len: Int): Boolean = {
    for (x: Int <- sx until sx+len) {
      for (offset: Array[Int] <- OFFSETS) {
        val x2: Int =  x + offset(0)
        val y2: Int = sy + offset(1)
        if (0 <= x2 && x2 < width && 0 <= y2 && y2 < height) {
          val c: Char = schematic(y2)(x2)
          if (c != '.' && (c < '0' || c > '9')) {
            return true
          }
        }
      }
    }
    return false
  }

  def solve(path: String): Int = {
    loadInput(path)

    var sum: Int = 0
    var y: Int = 0
    var x: Int = 0

    while (y < schematic.length) {
      while (x < schematic(y).length) {
        val c: Char = schematic(y)(x)
        if ('0' <= c && c <= '9') {
          val number: String = getNumber(x, y)
          if (isPartNumber(x, y, number.length)) {
            sum += number.toInt
            x += number.length - 1
          }
        }
        x += 1
      }
      y += 1
      x = 0
    }
    return sum
  }
  def main(args: Array[String]): Unit = {
    val solution: Int = solve("res/day3/input1.txt")
    println(solution)
  }
}
