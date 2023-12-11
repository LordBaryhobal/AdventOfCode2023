package day11

import org.scalatest.funsuite.AnyFunSuite

class Puzzle1Test extends AnyFunSuite {
  test("Puzzle1.solve") {
    assert(Puzzle1.solve("tests_res/day11/input1.txt") == 374)
  }
}
