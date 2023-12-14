package day14

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle2 {
  val CHARS: Array[Char] = Array('.', 'O', '#')
  var platform: Array[Array[Int]] = Array.empty
  var prevPlatform: Array[Array[Int]] = Array.empty
  var history: ArrayBuffer[Array[Array[Int]]] = new ArrayBuffer()
  var width: Int = 0
  var height: Int = 0

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    platform = lines.map(_.map(CHARS.indexOf(_)).toArray)
    height = platform.length
    width = if (height == 0) 0 else platform(0).length
    prevPlatform = Array.ofDim(height, width)

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

  def rotate(): Unit = {
    val result: Array[Array[Int]] = Array.ofDim(width, height)
    width += height
    height = width - height
    width = width - height

    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        result(y)(x) = platform(width-x-1)(y)
      }
    }
    platform = result
  }

  def cycle(): Unit = {
    slideUp()
    rotate()
    slideUp()
    rotate()
    slideUp()
    rotate()
    slideUp()
    rotate()
  }

  def copyPlatform(): Array[Array[Int]] = {
    val result: Array[Array[Int]] = Array.ofDim(height, width)
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        result(y)(x) = platform(y)(x)
      }
    }
    return result
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

  def getLoopStart(): Int = {
    for ((prevPlatform: Array[Array[Int]], i: Int) <- history.zipWithIndex) {
      if (noChange(prevPlatform)) return i
    }
    return -1
  }

  def noChange(prevPlatform: Array[Array[Int]]): Boolean = {
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (prevPlatform(y)(x) != platform(y)(x)) {
          return false
        }
      }
    }
    return true
  }

  def hasLooped(): Boolean = getLoopStart() != -1

  def repeatCycle(count: Int): Unit = {
    for (i: Int <- 0 until count) {
      history.prepend(copyPlatform())
      cycle()
      //display()
      if (hasLooped()) {
        val loopStart: Int = getLoopStart()
        val loopOffset: Int = (count - i - 1) % (loopStart + 1)
        val finalI: Int = loopStart - loopOffset

        platform = history(finalI)
        return
      }
    }
  }

  def display(): Unit = {
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        print(CHARS(platform(y)(x)))
      }
      println()
    }
    println()
  }

  def solve(path: String, cycles: Int): Int = {
    loadInput(path)
    repeatCycle(cycles)

    val solution: Int = computeLoad()

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day14/input1.txt", 1000000000)
    println(solution)
  }
}
