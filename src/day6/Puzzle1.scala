package day6

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  case class Race(duration: Int, record: Int)

  var races: Array[Race] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: Array[String] = source.getLines().toArray

    val durations: Array[Int] = lines(0).split(": +")(1).split(" +").map(d => d.toInt)
    val records: Array[Int] = lines(1).split(": +")(1).split(" +").map(r => r.toInt)

    races = new Array(durations.length)

    for (i: Int <- races.indices) {
      races(i) = Race(durations(i), records(i))
    }

    source.close()
  }

  def getMargin(race: Race): Int = {
    // dist = (dur - press) * press
    // dist-record > 0 <=> f(press) = dur*press - press^2 - record > 0
    // x^2 - d*x + r = 0
    // x = ( d +- sqrt(d^2 - 4r) ) / 2

    val delta: Double = race.duration*race.duration - 4*race.record
    val x0: Double = (race.duration - Math.sqrt(delta))/2
    val x1: Double = (race.duration + Math.sqrt(delta))/2

    val margin: Int = (Math.ceil(x1) - Math.floor(x0) - 1).toInt
    return margin
  }

  def solve(path: String): Int = {
    loadInput(path)

    var solution: Int = 1
    for (race: Race <- races) {
      solution *= getMargin(race)
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day6/input1.txt")
    println(solution)
  }
}
