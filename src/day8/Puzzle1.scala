package day8

import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

object Puzzle1 {
  var rule: Array[Int] = Array.empty
  val nodes: mutable.Map[String, Node] = new mutable.HashMap()
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

  def solve(path: String): Int = {
    loadInput(path)

    var solution: Int = 0
    var id: String = "AAA"

    while (id != "ZZZ") {
      id = nodes(id).nextNodes(rule(solution % rule.length))
      solution += 1
    }

    return solution
  }

  def main(args: Array[String]): Unit = {
    val solution: Int = solve("./res/day8/input1.txt")
    println(solution)
  }
}
