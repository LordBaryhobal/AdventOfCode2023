package day16

import day16.Mirrors.{DIAGONAL_BL_TR, DIAGONAL_TL_BR, Mirror, NONE, SPLITTER_HOR, SPLITTER_VER}

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle1 {
  var mirrors: Array[Array[Mirror]] = Array.empty
  var energized: Array[Array[Boolean]] = Array.empty
  var width: Int = 0
  var height: Int = 0
  val OFFSETS: Array[(Int, Int)] = Array(
    (1, 0), (0, 1), (-1, 0), (0, -1)
  )
  val path: ArrayBuffer[(Int, Int, Int)] = new ArrayBuffer()

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    height = lines.length
    width = if (height == 0) 0 else lines(0).length
    mirrors = Array.ofDim(height, width)
    energized = Array.ofDim(height, width)

    for ((line: String, y: Int) <- lines.zipWithIndex) {
      for ((c: Char, x: Int) <- line.zipWithIndex) {
        val mirror: Mirror = Mirrors.withName(""+c)
        mirrors(y)(x) = mirror
      }
    }

    source.close()
  }

  def shootBeam(startX: Int, startY: Int, startDir: Int): Unit = {
    if (startX < 0 || startX >= width || startY < 0 || startY >= height) return
    var x: Int = startX
    var y: Int = startY
    var dir: Int = startDir
    var continue: Boolean = true
    do {
      if (path.contains((x, y, dir))) return
      path.addOne((x, y, dir))

      energized(y)(x) = true

      mirrors(y)(x) match {
        case NONE => {}
        case DIAGONAL_TL_BR => {
          dir = Math.floorDiv(dir, 2)*2 + 1 - (dir % 2)
        }
        case DIAGONAL_BL_TR => {
          dir = 3 - dir
        }
        case SPLITTER_HOR => {
          if (dir % 2 == 1) {
            continue = false
            shootBeam(x+1, y, 0)
            shootBeam(x-1, y, 2)
          }
        }
        case SPLITTER_VER => {
          if (dir % 2 == 0) {
            continue = false
            shootBeam(x, y+1, 1)
            shootBeam(x, y-1, 3)
          }
        }
      }

      if (continue) {
        val offset: (Int, Int) = OFFSETS(dir)
        x += offset._1
        y += offset._2
      }

      if (x < 0 || x >= width) continue = false
      if (y < 0 || y >= height) continue = false
    } while (continue)
  }

  def solve(path: String): Int = {
    loadInput(path)
    shootBeam(0, 0, 0)

    var solution: Int = 0

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (energized(y)(x)) solution += 1
      }
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day16/input1.txt")
    println(solution)
  }
}
