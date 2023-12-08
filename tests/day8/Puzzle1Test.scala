package day8

import org.scalatest.funsuite.AnyFunSuite

class Puzzle1Test extends AnyFunSuite {
  test("Puzzle1.solve") {
    assert(Puzzle1.solve("tests_res/day8/input1.txt") == 6)
  }
}
