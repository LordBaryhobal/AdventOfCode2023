package day15

import scala.collection.mutable.ArrayBuffer

class Box(val id: Int) {
  var lenses: ArrayBuffer[Lens] = new ArrayBuffer()
  var lensesLabels: ArrayBuffer[String] = new ArrayBuffer()

  def addLens(lens: Lens): Unit = {
    if (lensesLabels.contains(lens.label)) {
      val i: Int = lensesLabels.indexOf(lens.label)
      lenses(i) = lens
    } else {
      lenses.addOne(lens)
      lensesLabels.addOne(lens.label)
    }
  }

  def removeLens(label: String): Unit = {
    if (lensesLabels.contains(label)) {
      val i: Int = lensesLabels.indexOf(label)
      lenses.remove(i)
      lensesLabels.remove(i)
    }
  }
  def getFocusingPower(): Long = {
    var power: Long = 0
    for ((lens: Lens, i: Int) <- lenses.zipWithIndex) {
      power += (i+1) * lens.focalLength
    }
    return power
  }
}
