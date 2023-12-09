package day8

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Ghost(val startId: String) extends Iterator[Long] {
  var preLoopOffsets: ArrayBuffer[Int] = new ArrayBuffer()
  var loopOffsets: ArrayBuffer[Int] = new ArrayBuffer()
  var visited: ArrayBuffer[(String, Int)] = new ArrayBuffer()
  private var distIterator: Iterator[Long] = Iterator.empty

  def computePath(nodes: mutable.Map[String, Node], rule: Array[Int]): Unit = {
    var id: String = startId
    preLoopOffsets.addOne(0)
    var ruleI: Int = 0

    // While not looped
    while (!visited.contains((id, ruleI))) {
      //println(s"  Visited $id (ruleI = $ruleI)")
      visited.addOne((id, ruleI))
      id = nodes(id).nextNodes(rule(ruleI))
      preLoopOffsets(preLoopOffsets.length-1) += 1
      ruleI = (ruleI + 1) % rule.length
      if (id.last == 'Z') {
        preLoopOffsets.addOne(0)
      }
    }

    val loopStartDist: Int = visited.indexOf((id, ruleI))
    var dist: Int = 0
    var loopStartI: Int = 0
    while (dist < loopStartDist) {
      dist += preLoopOffsets(loopStartI)
      loopStartI += 1
    }
    loopStartI -= 1
    preLoopOffsets(preLoopOffsets.length-1) += preLoopOffsets(loopStartI) - loopStartDist
    println(s"  Loop from ($id, $ruleI) (index=$loopStartI)")
    println(s"  # of offsets: ${preLoopOffsets.length}")

    loopOffsets.addAll(preLoopOffsets.drop(loopStartI+1))
    preLoopOffsets = preLoopOffsets.dropRight(preLoopOffsets.length - loopStartI - 1)
    println(preLoopOffsets.length)
    println(loopOffsets.length)
    distIterator = new GhostIterator(preLoopOffsets.toArray, loopOffsets.toArray)
  }

  override def hasNext: Boolean = true

  override def next(): Long = distIterator.next()

  private class GhostIterator(val preLoopOffsets: Array[Int], val loopOffsets: Array[Int]) extends Iterator[Long] {
    private var inLoop: Boolean = false
    private var offsetI: Int = 0
    private var dist: Long = 0
    override def hasNext: Boolean = true

    override def next(): Long = {
      var offset: Int = 0
      if (inLoop) {
        offset = loopOffsets(offsetI)
      } else {
        offset = preLoopOffsets(offsetI)
      }
      dist += offset

      offsetI += 1
      if (!inLoop) {
        if (offsetI == preLoopOffsets.length) {
          offsetI = 0
          inLoop = true
        }
      } else {
        offsetI %= loopOffsets.length
      }

      return dist
    }
  }
}
