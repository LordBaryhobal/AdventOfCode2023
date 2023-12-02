package day2

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.HashMap

class Puzzle1Test extends AnyFunSuite {
  test("Puzzle1.solve") {
    assert(Puzzle1.solve("tests_res/day2/input1.txt", HashMap(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )) == 8)
  }
}
