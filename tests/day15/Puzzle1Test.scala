package day15

import org.scalatest.funsuite.AnyFunSuite

class Puzzle1Test extends AnyFunSuite {
  test("Puzzle1.solve") {
    assert(Puzzle1.solve("tests_res/day15/input1.txt") == 1320)
  }
}
