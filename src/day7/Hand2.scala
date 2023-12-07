package day7

import scala.collection.mutable

class Hand2(var cards: Array[Int], val bid: Int) {

  def getType(): Int = {
    val cardsMap: mutable.Map[Int, Int] = new mutable.HashMap()
    val jokers: Int = cards.count(c => c == 0)
    for (card: Int <- cards) {
      if (card != 0) {
        if (!cardsMap.contains(card)) {
          cardsMap(card) = 0
        }
        cardsMap(card) += 1
      }
    }

    val keys: Array[Int] = cardsMap.keys.toArray
    val values: Array[Int] = cardsMap.values.toArray

    if (jokers == 5) return 6

    // Five of a kind
    for (value: Int <- values) {
      if (value + jokers == 5) return 6
    }

    // Four of a kind
    for (value: Int <- values) {
      if (value + jokers == 4) return 5
    }

    // Full house
    val sortedValues: Array[Int] = values.sorted(Ordering.Int.reverse)
    if (sortedValues(0) + sortedValues(1) + jokers >= 5) return 4

    // Three of a kind
    if (sortedValues(0) + jokers >= 3) return 3

    // Two pairs
    if (sortedValues(0) + sortedValues(1) + jokers >= 4) return 2

    // One pair
    if (sortedValues(0) + jokers >= 2) return 1

    return 0
  }

  def isBetter(hand: Hand2): Boolean = {
    val t1: Int = getType()
    val t2: Int = hand.getType()
    if (t1 > t2) return true
    if (t1 < t2) return false

    for ((c1: Int, c2: Int) <- cards.zip(hand.cards)) {
      if (c1 != c2) {
        return c1 > c2
      }
    }
    return false
  }

  override def toString: String = s"Hand(${cards.mkString}, $bid, ${getType()})"
}
