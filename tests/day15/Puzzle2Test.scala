package day15

import org.scalatest.funsuite.AnyFunSuite

class Puzzle2Test extends AnyFunSuite {
  test("Puzzle2.solve") {
    assert(Puzzle2.solve("tests_res/day15/input1.txt") == 145)
  }
}
