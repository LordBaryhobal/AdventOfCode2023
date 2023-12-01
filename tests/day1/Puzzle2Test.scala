package day1

import org.scalatest.funsuite.AnyFunSuite

class Puzzle2Test extends AnyFunSuite {

  test("Puzzle2.loadInput") {
    assert(Puzzle2.loadInput("tests_res/day1/input2.txt") == 281)
  }

}
