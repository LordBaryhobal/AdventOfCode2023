package day7

import org.scalatest.funsuite.AnyFunSuite

class Puzzle2Test extends AnyFunSuite {
  test("Puzzle2.solve") {
    assert(Puzzle2.solve("tests_res/day7/input1.txt") == 5905)
  }
}
