package day5

import scala.io.{BufferedSource, Source}

object Puzzle1 {
  case class Range(startSrc: Long, startDst: Long, length: Long)
  var forwardingTables: Array[Array[Range]] = Array.empty
  var seeds: Array[Long] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)

    val almanac: String = source.getLines().mkString("\n")
    val tables: Array[String] = almanac.split("\n\n")

    forwardingTables = new Array(tables.length - 1)
    seeds = tables(0).substring(7).split(" ").map(s => s.toLong)

    for (i: Int <- 1 until tables.length) {
      val lines: Array[String] = tables(i).split(":\n")(1).split("\n")
      val table: Array[Range] = new Array(lines.length)
      for ((line: String, j: Int) <- lines.zipWithIndex) {
        val values: Array[Long] = line.split(" ").map(v => v.toLong)
        table(j) = Range(values(1), values(0), values(2))
      }
      forwardingTables(i-1) = table
    }

    source.close()
  }

  def sortTables(): Unit = {
    for (table: Array[Range] <- forwardingTables) {
      var changed: Boolean = false
      do {
        changed = false
        for (i: Int <- 0 until table.length-1) {
          if (table(i).startSrc >= table(i + 1).startSrc + table(i + 1).length) {
            changed = true
            val range: Range = table(i)
            table(i) = table(i + 1)
            table(i + 1) = range
          }
        }
      } while (changed)
    }
  }
  def map(seed: Long): Long = {
    var value: Long = seed
    for (i: Int <- forwardingTables.indices) {
      value = forward(value, i)
    }
    return value
  }

  def forward(value: Long, tableI: Int): Long = {
    for (range: Range <- forwardingTables(tableI)) {
      if (value >= range.startSrc) {
        if (value < range.startSrc + range.length) {
          return value - range.startSrc + range.startDst
        }
      } else {
        return value
      }
    }
    return value
  }
  def solve(path: String): Long = {
    loadInput(path)
    sortTables()
    var smallest: Long = -1
    for (seed: Long <- seeds) {
      val location: Long = map(seed)
      if (smallest == -1 || location < smallest) {
        smallest = location
      }
    }
    return smallest
  }
  def main(args: Array[String]): Unit = {
    val solution: Long = solve("res/day5/input1.txt")
    println(solution)
  }
}
