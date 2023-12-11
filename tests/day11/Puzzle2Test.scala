package day11

import org.scalatest.funsuite.AnyFunSuite

class Puzzle2Test extends AnyFunSuite {
  test("Puzzle2.solve 1") {
    assert(Puzzle2.solve("tests_res/day11/input1.txt", 10) == 1030)
  }
  test("Puzzle2.solve 2") {
    assert(Puzzle2.solve("tests_res/day11/input1.txt", 100) == 8410)
  }
}
