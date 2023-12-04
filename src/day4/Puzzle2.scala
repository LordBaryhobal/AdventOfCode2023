package day4

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

object Puzzle2 {
  case class Card(id: Int, validNumbers: Array[Int], myNumbers: Array[Int], var score: Int = 0, var count: Int = 1)

  var cards: Array[Card] = Array.empty

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)

    val lines: Array[String] = source.getLines().toArray
    cards = new Array(lines.length)

    for ((line: String, i: Int) <- lines.zipWithIndex) {
      val parts: Array[String] = line.split(": +")(1).split(" +\\| +")
      val validNumbers: Array[Int] = parts(0).split(" +").map(n => n.toInt)
      val myNumbers: Array[Int] = parts(1).split(" +").map(n => n.toInt)
      cards(i) = Card(i, validNumbers, myNumbers)
    }

    source.close()
  }

  def computeScores(): Unit = {
    for (card: Card <- cards) {
      card.score = card.validNumbers.intersect(card.myNumbers).length
    }
  }
  def solve(path: String): Int = {
    loadInput(path)
    computeScores()
    var count: Int = 0
    for (card: Card <- cards) {
      count += card.count
      for (i: Int <- 0 until card.score) {
        cards(card.id + i + 1).count += card.count
      }
    }
    return count
  }
  def main(args: Array[String]): Unit = {
    val solution: Int = solve("res/day4/input1.txt")
    println(solution)
  }
}
