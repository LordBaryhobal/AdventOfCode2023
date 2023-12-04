package day4

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  case class Card(validNumbers: Array[Int], myNumbers: Array[Int])

  var cards: Array[Card] = Array.empty

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)

    val lines: Array[String] = source.getLines().toArray
    cards = new Array(lines.length)

    for ((line: String, i: Int) <- lines.zipWithIndex) {
      val parts: Array[String] = line.split(": +")(1).split(" +\\| +")
      val validNumbers: Array[Int] = parts(0).split(" +").map(n => n.toInt)
      val myNumbers: Array[Int] = parts(1).split(" +").map(n => n.toInt)
      cards(i) = Card(validNumbers, myNumbers)
    }

    source.close()
  }

  def computeGain(): Int = {
    var gain: Int = 0
    for (card: Card <- cards) {
      val validCount: Int = card.validNumbers.intersect(card.myNumbers).length
      if (validCount >= 1) {
        gain += Math.pow(2, validCount-1).toInt
      }
    }
    return gain
  }
  def solve(path: String): Int = {
    loadInput(path)
    return computeGain()
  }
  def main(args: Array[String]): Unit = {
    val solution: Int = solve("res/day4/input1.txt")
    println(solution)
  }
}
