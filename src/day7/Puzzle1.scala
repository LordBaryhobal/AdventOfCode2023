package day7

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  val CARDS: Array[Int] = Array('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A')
  var hands: Array[Hand] = Array.empty

  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    hands = new Array(lines.length)

    for ((line: String, i: Int) <- lines.zipWithIndex) {
      val parts: Array[String] = line.split(" ")
      val cards: Array[Int] = parts(0).map(c => CARDS.indexOf(c)).toArray
      val bid: Int = parts(1).toInt
      hands(i) = new Hand(cards, bid)
    }

    source.close()
  }

  def sortHands(): Unit = {
    hands = hands.sortWith((h1: Hand, h2: Hand) => h2.isBetter(h1))
  }

  def solve(path: String): Int = {
    loadInput(path)
    sortHands()

    var solution: Int = 0

    for ((hand: Hand, i: Int) <- hands.zipWithIndex) {
      solution += hand.bid * (i+1)
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day7/input1.txt")
    println(solution)
  }
}
