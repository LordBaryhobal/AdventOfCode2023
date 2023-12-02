package day2

import scala.collection.immutable.HashMap
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle1 {
  var games: Array[String] = new Array(0)
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    games = source.getLines().toArray
    source.close()
  }
  def getPossible(content: Map[String, Int]): ArrayBuffer[Int] = {
    val result: ArrayBuffer[Int] = new ArrayBuffer()

    for (game: String <- games) {
      val parts: Array[String] = game.split(": ")
      val gameId: Int = parts(0).split(" ")(1).toInt
      val sets: Array[String] = parts(1).split("; ")

      if (isGamePossible(sets, content)) {
        result.addOne(gameId)
      }
    }
    return result
  }

  def isGamePossible(sets: Array[String], content: Map[String, Int]): Boolean = {
      for (set: String <- sets) {
        val groups: Array[String] = set.split(", ")
        for (group: String <- groups) {
          val parts: Array[String] = group.split(" ")
          val count: Int = parts(0).toInt
          val color: String = parts(1)

          if (content(color) < count) return false
        }
      }

      return true
  }

  def solve(path: String, content: Map[String, Int]): Int = {
    loadInput(path)
    val possibleGames: Array[Int] = getPossible(content).toArray
    return possibleGames.sum
  }
  def main(args: Array[String]): Unit = {
    val solution: Int = solve("res/day2/input1.txt", HashMap(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    ))
    println(solution)
  }
}
