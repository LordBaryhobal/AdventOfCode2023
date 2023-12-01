package day1

import org.scalatest.funsuite.AnyFunSuite

class Puzzle1Test extends AnyFunSuite {
  test("Puzzle1.loadInput") {
    assert(Puzzle1.loadInput("tests_res/day1/input1.txt") == 142)
  }
}