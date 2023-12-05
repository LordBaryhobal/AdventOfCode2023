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

    /**
     * Sorts the ranges in ascending order
     */
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

    /**
     * Fills the gaps between ranges with identity ranges (i.e. which keep values unchanged)
     */
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

    /**
     * Forwards a value through this table
     * @param src The source to forward
     * @return A pair containing the destination value and new table id
     */
    def forward(src: Long): (Long, Int) = {
      for (range: Range <- ranges) {
        if (range.containsSrc(src)) return (range.forward(src), range.dstTable)
      }
      return (src, id+1)
    }

    /**
     * Forwards a value in reverse through this table
     * @param dst The destination to "backward"
     * @return A pair containing the source value and new table id
     */
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

  /**
   * Reorganizes all the tables by sorting the ranges and filling the gaps between them
   */
  def sortTables(): Unit = {
    for (table: Table <- forwardingTables) {
      table.sortRanges()
      table.fillGaps()
    }
  }

  /**
   * Collapses all forwarding tables into a single one, mapping seeds to locations
   */
  def collapseTables(): Unit = {
    for (i: Int <- forwardingTables.length - 1 until 0 by -1) {
      val newTable: Table = collapseTable(forwardingTables(i-1), forwardingTables(i))
      forwardingTables(i-1) = newTable
    }
  }

  /**
   * Collapses a forwarding table with the next tables
   * @param table1 The table to collapse
   * @param table2 The next table
   * @return A new table equivalent to the fusion of both tables
   */
  def collapseTable(table1: Table, table2: Table): Table = {
    // Find all new range limits
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

    // For each limit, create the corresponding range by forwarding table 1 through table 2
    for (i: Int <- 0 until limits.length - 1) {
      val src1: Long = limits(i)
      val dst1: (Long, Int) = table1.forward(src1)
      if (dst1._2 != table2.id) throw new Exception("Something weird happened")
      val dst2: (Long, Int) = table2.forward(dst1._1)
      newTable.addRange(Range(src1, dst2._1, limits(i+1)-src1, dst2._2))
    }
    return newTable
  }

  def solve(path: String): Long = {
    println("Solving puzzle 2")
    loadInput(path)
    println("Loaded input")
    sortTables()
    /*for (table: Table <- forwardingTables) {
      println(table)
    }*/
    println("Sorted forwarding tables")
    collapseTables()
    println("Simplified forwarding tables")
    val table: Table = forwardingTables(0)
    //println(table)

    var smallest: Long = -1
    for (i: Int <- seeds.indices by 2) {
      val seedStart: Long = seeds(i)
      val seedEnd: Long = seedStart + seeds(i+1) - 1

      // Find range limits lying inside seed range
      val limitsSet: mutable.Set[Long] = new mutable.HashSet()
      limitsSet.addOne(seedStart)
      limitsSet.addOne(seedEnd)
      for (r2: Range <- table.ranges) {
        if (r2.startSrc > seedStart && r2.startSrc < seedEnd) limitsSet.add(r2.startSrc)
        if (r2.endSrc > seedStart && r2.endSrc < seedEnd) limitsSet.add(r2.startSrc)
      }
      val limits: Array[Long] = limitsSet.toArray.sorted

      // For each limit, forward its value and compare with current smallest
      for (i: Int <- 0 until limits.length - 1) {
        val value: Long = table.forward(limits(i))._1
        if (smallest == -1 || value < smallest) smallest = value
      }
    }
    return smallest
  }
  def main(args: Array[String]): Unit = {
    val solution: Long = solve("res/day5/input1.txt")
    println(solution)
  }
}