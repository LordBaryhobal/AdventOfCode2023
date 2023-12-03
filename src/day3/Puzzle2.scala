package day3

import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle2 {
  case class Number(x: Int, y: Int, value: Int, length: Int)
  case class Gear(x: Int, y: Int, var ratio: Int = 1, var neighbours: Int = 0)

  var width = 0
  var height = 0

  var schematic: Array[Array[Char]] = Array.empty
  var numbers: ArrayBuffer[Number] = new ArrayBuffer()
  var gears: ArrayBuffer[Gear] = new ArrayBuffer()

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

  def findNumbers(): Unit = {
    var y: Int = 0
    var x: Int = 0

    while (y < height) {
      while (x < width) {
        val c: Char = schematic(y)(x)
        if ('0' <= c && c <= '9') {
          val number: String = getNumber(x, y)
          numbers.addOne(Number(x, y, number.toInt, number.length))
          x += number.length - 1
        }
        x += 1
      }
      y += 1
      x = 0
    }
  }

  def findGears(): Unit = {
    for (y: Int <- 0 until height) {
      for (x: Int <- 0 until width) {
        if (schematic(y)(x) == '*') {
          gears.addOne(Gear(x, y))
        }
      }
    }
  }

  def computePowers(): Unit = {
    for (gear: Gear <- gears) {
      for (number: Number <- numbers) {
        if (areNeighbours(gear, number)) {
          gear.ratio *= number.value
          gear.neighbours += 1
        }
      }
    }
  }

  def areNeighbours(gear: Gear, number: Number): Boolean = {
    if (gear.x > number.x + number.length) return false
    if (gear.y > number.y + 1) return false
    if (number.x > gear.x + 1) return false
    if (number.y > gear.y + 1) return false
    return true
  }

  def solve(path: String): Int = {
    loadInput(path)
    findNumbers()
    findGears()
    computePowers()
    return gears.filter(g => g.neighbours == 2).map(g => g.ratio).sum
  }
  def main(args: Array[String]): Unit = {
    val solution: Int = solve("res/day3/input1.txt")
    println(solution)
  }
}
