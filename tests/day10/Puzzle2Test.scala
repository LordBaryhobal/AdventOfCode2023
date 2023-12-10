package day10

import org.scalatest.funsuite.AnyFunSuite

class Puzzle2Test extends AnyFunSuite {
  test("Puzzle2.solve 1") {
    assert(Puzzle2.solve("tests_res/day10/input1.txt") == 1)
  }
  test("Puzzle2.solve 2") {
    assert(Puzzle2.solve("tests_res/day10/input2.txt") == 1)
  }
  test("Puzzle2.solve 3") {
    assert(Puzzle2.solve("tests_res/day10/input3.txt") == 4)
  }
  test("Puzzle2.solve 4") {
    assert(Puzzle2.solve("tests_res/day10/input4.txt") == 4)
  }
  test("Puzzle2.solve 5") {
    assert(Puzzle2.solve("tests_res/day10/input5.txt") == 8)
  }
  test("Puzzle2.solve 6") {
    assert(Puzzle2.solve("tests_res/day10/input6.txt") == 10)
  }
}
