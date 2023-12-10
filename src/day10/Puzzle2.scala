package day10

import util.Ansi

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle2 {
  var grid: Array[Array[Byte]] = Array.empty
  var zones: Array[Array[Int]] = Array.empty

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

  val TILE_CHARS: Map[Int, Char] = HashMap(
    10 -> '│',
    5 -> '─',
    9 -> '└',
    12 -> '┘',
    6 -> '┐',
    3 -> '┌',
    0 -> ' ',
    15 -> '█'
  )

  val TILE_CHARS_BOLD: Map[Int, Char] = HashMap(
    10 -> '┃',
    5 -> '━',
    9 -> '┗',
    12 -> '┛',
    6 -> '┓',
    3 -> '┏',
    0 -> ' ',
    15 -> '█'
  )

  var height: Int = 0
  var width: Int = 0
  var startX: Int = 0
  var startY: Int = 0
  var path: ArrayBuffer[(Int, Int)] = new ArrayBuffer()

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    height = lines.length
    width = if (height == 0) 0 else lines(0).length
    grid = Array.ofDim(height, width)
    zones = Array.ofDim(height, width)

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

  def display(): Unit = {
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        val tile: Byte = grid(y)(x)
        val zone: Int = zones(y)(x)
        if (zone == 1) {
          print(Ansi.BG_RGB(163, 61, 61))
        } else if (zone == 2) {
          print(Ansi.BG_RGB(77, 163, 61))
        }
        if (path.contains((x, y))) {
          print(Ansi.BOLD)
          print(TILE_CHARS_BOLD(tile))
        } else {
          print(TILE_CHARS(tile))
        }
        print(Ansi.CLEAR)
      }
      println()
    }
  }

  def calculateArea(): Int = {
    path = new ArrayBuffer()
    path.addOne((startX, startY))

    val walker: Walker = new Walker(startX, startY)

    do {
      walker.walk(grid)
      path.addOne(walker.getPos())
    } while (walker.getX() != startX || walker.getY() != startY)
    println(s"Found path (length = ${path.length})")

    val painter: Painter = new Painter(startX, startY, 2*startX-path(1)._1, 2*startY-path(1)._2)
    do {
      painter.walk(grid, zones, path)
    } while (painter.getX() != startX || painter.getY() != startY)
    println("Painted path neighbours")

    var newZones: Array[Array[Int]] = Array.ofDim(height, width)
    var changed: Boolean = true
    var exteriorZone: Int = 0

    do {
      changed = false
      newZones = copyZones()
      for (y: Int <- 0 until height) {
        for (x: Int <- 0 until width) {
          if (zones(y)(x) != 0) {
            if (floodTile(x, y, newZones, path)) {
              changed = true
            }
          }
        }
      }
      zones = newZones
    } while (changed)
    println("Flooded zones")

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (x == 0 || x == width - 1 || y == 0 || y == height - 1) {
          if (zones(y)(x) != 0) {
            exteriorZone = zones(y)(x)
          }
        }
      }
    }
    println(s"Found exterior zone: $exteriorZone")

    var area: Int = 0
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (zones(y)(x) != exteriorZone && zones(y)(x) != 0) area += 1
      }
    }
    println(s"Calculated area: $area")

    return area
  }

  def floodTile(x: Int, y: Int, newZones: Array[Array[Int]], path: ArrayBuffer[(Int, Int)]): Boolean = {
    var changed: Boolean = false
    for ((dx: Int, dy: Int) <- Walker.OFFSETS) {
      val x2: Int = x + dx
      val y2: Int = y + dy
      if (0 <= x2 && x2 < width && 0 <= y2 && y2 < height) {
        if (zones(y2)(x2) == 0 && !path.contains((x2, y2))) {
          newZones(y2)(x2) = zones(y)(x)
          changed = true
        }
      }
    }
    return changed
  }

  def copyZones(): Array[Array[Int]] = {
    val result: Array[Array[Int]] = Array.ofDim(height, width)
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        result(y)(x) = zones(y)(x)
      }
    }
    return result
  }

  def solve(path: String): Int = {
    loadInput(path)

    val solution: Int = calculateArea()

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day10/input1.txt")
    println(solution)
    display()
  }
}
