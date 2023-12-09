package day8

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Puzzle2 {
  var rule: Array[Int] = Array.empty
  val nodes: mutable.Map[String, Node] = new mutable.HashMap()
  var ghosts: Array[Ghost] = Array.empty
  def loadInput(path: String): Unit = {
    val source: BufferedSource = Source.fromFile(path)
    val lines: String = source.getLines().mkString("\n")
    val parts: Array[String] = lines.split("\n\n")

    rule = parts(0).map(c => "LR".indexOf(c)).toArray
    val regexp: Regex = new Regex("([A-Z]{3}) = \\(([A-Z]{3}), ([A-Z]{3})\\)")
    for (line: String <- parts(1).split("\n")) {
      val matches: Match = regexp.findFirstMatchIn(line).get
      val id: String = matches.group(1)
      val left: String = matches.group(2)
      val right: String = matches.group(3)
      nodes(id) = new Node(id, Array(left, right))
    }

    source.close()
  }

  def solve(path: String): Long = {
    loadInput(path)

    ghosts = nodes.keys.filter(k => k.last == 'A').map(k => new Ghost(k)).toArray

    val ghostDists: mutable.Map[Ghost, Long] = new mutable.HashMap()
    for (ghost: Ghost <- ghosts) {
      println(s"Ghost from ${ghost.startId}")
      ghost.computePath(nodes, rule)
      ghostDists(ghost) = ghost.next()
    }

    while (ghostDists.values.toArray.distinct.length != 1) {
      val minimum: (Ghost, Long) = ghostDists.toArray.minBy(_._2)
      ghostDists(minimum._1) = minimum._1.next()
    }

    val dists: Array[Long] = ghostDists.values.toArray

    return dists(0)
  }

  def main(args: Array[String]): Unit = {
    val solution: Long = solve("./res/day8/input1.txt")
    println(solution)
  }
}
