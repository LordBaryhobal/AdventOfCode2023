package day5

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.{BufferedSource, Source}

object Puzzle2 {
  case class Range(startSrc: Long, startDst: Long, length: Long, dstTable: Int) {
    def endSrc: Long = startSrc + length
    def endDst: Long = startDst + length
    def containsSrc(src: Long): Boolean = startSrc <= src && src < endSrc
    def containsDst(dst: Long): Boolean = startDst <= dst && dst < endDst
    def forward(src: Long): Long = startDst + src - startSrc
    def backward(dst: Long): Long = startSrc + dst - startDst
    override def toString: String = s"[$startSrc;$endSrc[ -> ($dstTable) [$startDst;$endDst["
  }
  class Table(val id: Int) {
    var ranges: ArrayBuffer[Range] = new ArrayBuffer()

    def addRange(range: Range): Unit = ranges.addOne(range)
    def sortRanges(): Unit = {
      var changed: Boolean = false
      do {
        changed = false
        for (i: Int <- 0 until ranges.length-1) {
          if (ranges(i).startSrc >= ranges(i + 1).startSrc + ranges(i + 1).length) {
            changed = true
            val range: Range = ranges(i)
            ranges(i) = ranges(i + 1)
            ranges(i + 1) = range
          }
        }
      } while (changed)
    }

    def fillGaps(): Unit = {
      var r1: Range = Range(0, 0, 0, id + 1)
      var r2: Range = r1
      var i: Int = 0
      while (i < ranges.length) {
        r2 = ranges(i)
        val start: Long = r1.startSrc+r1.length
        if (start != r2.startSrc) {
          ranges.insert(i, Range(start, start, r2.startSrc - start, id+1))
          i += 1
        }
        r1 = r2
        i += 1
      }
    }

    def forward(src: Long): (Long, Int) = {
      for (range: Range <- ranges) {
        if (range.containsSrc(src)) return (range.forward(src), range.dstTable)
      }
      return (src, id+1)
    }
    def backward(dst: Long): (Long, Int) = {
      for (range: Range <- ranges) {
        if (range.containsDst(dst)) return (range.backward(dst), id)
      }
      return (dst, id)
    }

    override def toString: String = s"Table $id:\n  " + ranges.mkString("\n  ")
  }

  var forwardingTables: Array[Table] = Array.empty
  var seeds: Array[Long] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)

    val almanac: String = source.getLines().mkString("\n")
    val tables: Array[String] = almanac.split("\n\n")

    forwardingTables = new Array(tables.length - 1)
    seeds = tables(0).substring(7).split(" ").map(s => s.toLong)

    for (i: Int <- 1 until tables.length) {
      val lines: Array[String] = tables(i).split(":\n")(1).split("\n")
      val table: Table = new Table(i-1)
      for (line: String <- lines) {
        val values: Array[Long] = line.split(" ").map(v => v.toLong)
        table.addRange(Range(values(1), values(0), values(2), i))
      }
      forwardingTables(i-1) = table
    }

    source.close()
  }

  def sortTables(): Unit = {
    for (table: Table <- forwardingTables) {
      table.sortRanges()
      table.fillGaps()
    }
  }

  def simplifyTables(): Unit = {
    for (i: Int <- forwardingTables.length - 1 until 0 by -1) {
      //simplifyTable(i)
      val newTable: Table = collapseTables(forwardingTables(i-1), forwardingTables(i))
      forwardingTables(i-1) = newTable
    }
  }

  def simplifyTable(tableI: Int): Unit = {
    val table1: Table = forwardingTables(tableI)
    val table2: Table = forwardingTables(tableI+1)

    val newTable1: Table = new Table(table1.id)
    val limits: ArrayBuffer[Long] = new ArrayBuffer()

    for (r1: Range <- table1.ranges) {
      val startFrom1 = r1.startSrc
      val endFrom1 = startFrom1 + r1.length
      val startTo1 = r1.startDst
      val endTo1 = startTo1 + r1.length

      limits.addOne(startTo1)
      limits.addOne(endTo1)

      for (r2: Range <- table2.ranges) {
        val startFrom2 = r2.startSrc
        val endFrom2 = startFrom2 + r2.length
        val startTo2 = r2.startDst
        val endTo2 = startTo2 + r2.length

        limits.addOne(startFrom2)
        limits.addOne(endFrom2)

        // If overlap
        if (!(endTo1 < startFrom2 || startTo1 > endFrom2)) {
          
        }
      }
    }

    forwardingTables(tableI) = newTable1
  }

  def collapseTables(table1: Table, table2: Table): Table = {
    val limitsSet: mutable.Set[Long] = new mutable.HashSet()
    limitsSet.addOne(table1.ranges(0).startSrc)
    for (range: Range <- table1.ranges) {
      limitsSet.addOne(range.endSrc)
    }
    limitsSet.addOne(table1.backward(table2.ranges(0).startSrc)._1)
    for (range: Range <- table2.ranges) {
      limitsSet.addOne(table1.backward(range.endSrc)._1)
    }

    val newTable: Table = new Table(table1.id)
    val limits: Array[Long] = limitsSet.toArray.sorted
    for (i: Int <- 0 until limits.length - 1) {
      val src1: Long = limits(i)
      val dst1: (Long, Int) = table1.forward(src1)
      if (dst1._2 != table2.id) throw new Exception("Something weird happened")
      val dst2: (Long, Int) = table2.forward(dst1._1)
      newTable.addRange(Range(src1, dst2._1, limits(i+1)-src1, dst2._2))
    }
    return newTable
  }

  def forward(value: Long, tableI: Int): Long = {
    for (range: Range <- forwardingTables(tableI).ranges) {
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
    println("Solving puzzle 2")
    loadInput(path)
    println("Loaded input")
    sortTables()
    for (table: Table <- forwardingTables) {
      println(table)
    }
    println("Sorted forwarding tables")
    simplifyTables()
    println("Simplified forwarding tables")
    val table: Table = forwardingTables(0)
    println(table)

    var smallest: Long = -1
    for (i: Int <- seeds.indices by 2) {
      println(s"Mapping range of seed [${seeds(i)},${seeds(i)+seeds(i+1)}]")
      for (seed: Long <- seeds(i) until seeds(i)+seeds(i+1)) {
        val location: Long = table.forward(seed)._1
        if (smallest == -1 || location < smallest) {
          smallest = location
        }
      }
      println(s"New smallest location: $smallest")
    }
    return smallest
  }
  def main(args: Array[String]): Unit = {
    val solution: Long = solve("res/day5/input1.txt")
    println(solution)
  }
}