package day2

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

object Puzzle2 {
  var games: Array[String] = new Array(0)
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    games = source.getLines().toArray
    source.close()
  }

  def getPowers(): Array[Int] = {
    val powers: Array[Int] = new Array(games.length)
    for ((game: String, i: Int) <- games.zipWithIndex) {
      powers(i) = getPower(game)
    }
    return powers
  }

  def getPower(game: String): Int = {
    var power: Int = 1

    val sets: Array[String] = game.split(": ")(1).split("; ")
    val minCounts: mutable.Map[String, Int] = new mutable.HashMap()

    for (set: String <- sets) {
      val groups: Array[String] = set.split(", ")
      for (group: String <- groups) {
        val parts: Array[String] = group.split(" ")
        val count: Int = parts(0).toInt
        val color: String = parts(1)

        if (minCounts.contains(color)) {
          minCounts(color) = math.max(minCounts(color), count)
        } else {
          minCounts(color) = count
        }
      }
    }

    minCounts.values.foreach((count: Int) => power *= count)
    return power
  }

  def solve(path: String): Int = {
    loadInput(path)
    val powers: Array[Int] = getPowers()
    return powers.sum
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("res/day2/input1.txt")
    println(solution)
  }
}
