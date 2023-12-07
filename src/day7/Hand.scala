package day7

import scala.collection.mutable

class Hand(var cards: Array[Int], val bid: Int) {
  def getType(): Int = {
    val cardsMap: mutable.Map[Int, Int] = new mutable.HashMap()
    for (card: Int <- cards) {
      if (!cardsMap.contains(card)) {
        cardsMap(card) = 0
      }
      cardsMap(card) += 1
    }

    val keys: Array[Int] = cardsMap.keys.toArray
    val values: Array[Int] = cardsMap.values.toArray
    if (keys.length == 1) {
      return 6
    }
    if (keys.length == 2) {
      if (values.contains(4)) return 5
      if (values.contains(3) && values.contains(2)) return 4
    }
    if (values.contains(3)) return 3
    if (values.contains(2)) {
      if (values.count(v => v == 2) == 2) return 2
      return 1
    }
    return 0
  }

  def isBetter(hand: Hand): Boolean = {
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
