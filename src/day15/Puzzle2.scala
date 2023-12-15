package day15

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Puzzle2 {
  var steps: Array[String] = Array.empty
  val boxes: Array[Box] = new Array(256)
  val regex: Regex = new Regex("([a-z]+)(-|=\\d)")
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val line: String = source.getLines().mkString

    for (i: Int <- 0 until 256) {
      boxes(i) = new Box(i)
    }

    steps = line.split(",")

    source.close()
  }

  def hash(str: String): Int = {
    var value: Int = 0
    for (c: Char <- str) {
      value += c
      value *= 17
      value %= 256
    }
    return value
  }

  def getFocusingPower(): Long = {
    var power: Long = 0
    for (box: Box <- boxes) {
      power += (box.id+1) * box.getFocusingPower()
    }
    return power
  }

  def placeLenses(): Unit = {
    for (step: String <- steps) {
      val m: Match = regex.findFirstMatchIn(step).get
      val label: String = m.group(1)
      val labelHash: Int = hash(label)
      val action: Char = m.group(2)(0)
      val box: Box = boxes(labelHash)
      if (action == '=') {
        val lens: Lens = new Lens(label, m.group(2)(1) - '0')
        box.addLens(lens)
      } else {
        box.removeLens(label)
      }
    }
  }

  def solve(path: String): Long = {
    loadInput(path)
    placeLenses()

    val solution: Long = getFocusingPower()

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Long = solve("./res/day15/input1.txt")
    println(solution)
  }
}
