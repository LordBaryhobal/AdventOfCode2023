package day13

import scala.io.{BufferedSource, Source}

object Puzzle2 {
  var maps: Array[Array[Array[Boolean]]] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: String = source.getLines().mkString("\n")

    val mapsStr: Array[String] = lines.split("\n\n")
    maps = new Array(mapsStr.length)

    for ((mapStr: String, i: Int) <- mapsStr.zipWithIndex) {
      val rows: Array[String] = mapStr.split("\n")
      val map: Array[Array[Boolean]] = new Array(rows.length)
      for ((row: String, j: Int) <- rows.zipWithIndex) {
        map(j) = row.map(_ == '#').toArray
      }
      maps(i) = map
    }

    source.close()
  }

  def getReflectionValue(map: Array[Array[Boolean]]): Int = {
    val height: Int = map.length
    val width: Int = if (height == 0) 0 else map(0).length
    var value: Int = 0

    for (x: Int <- 0 until width-1) {
      if (isReflection(map, x)) {
        value += x+1
      }
    }

    val mapT: Array[Array[Boolean]] = transpose(map)

    for (y: Int <- 0 until height-1) {
      if (isReflection(mapT, y)) {
        value += 100*(y+1)
      }
    }

    return value
  }

  def isReflection(map: Array[Array[Boolean]], x: Int): Boolean = {
    var diffs: Int = 0
    for (row: Array[Boolean] <- map) {
      val a = row.dropRight(row.length - x - 1).reverse
      val b = row.drop(x+1)
      for (i: Int <- 0 until Math.min(a.length, b.length)) {
        if (a(i) != b(i)) {
          diffs += 1
          if (diffs > 1) return false
        }
      }
    }
    return diffs == 1
  }

  def transpose(map: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val height: Int = map.length
    val width: Int = if (height == 0) 0 else map(0).length

    val mapT: Array[Array[Boolean]] = Array.ofDim(width, height)

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        mapT(x)(y) = map(y)(x)
      }
    }

    return mapT
  }

  def solve(path: String): Int = {
    loadInput(path)

    var solution: Int = 0

    for (map: Array[Array[Boolean]] <- maps) {
      solution += getReflectionValue(map)
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day13/input1.txt")
    println(solution)
  }
}
