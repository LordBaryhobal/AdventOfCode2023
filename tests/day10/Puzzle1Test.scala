package day10

import org.scalatest.funsuite.AnyFunSuite

class Puzzle1Test extends AnyFunSuite {
  test("Puzzle1.solve 1") {
    assert(Puzzle1.solve("tests_res/day10/input1.txt") == 4)
  }
  test("Puzzle1.solve 2") {
    assert(Puzzle1.solve("tests_res/day10/input2.txt") == 8)
  }
}
