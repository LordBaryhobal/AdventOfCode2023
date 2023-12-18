package day18

import scala.collection.mutable.ArrayBuffer

class Painter(startX: Int, startY: Int, excludeX: Int, excludeY: Int) extends Walker(startX, startY, excludeX, excludeY) {
  def walk(grid: Array[Array[Byte]], zones: Array[Array[Int]], path: ArrayBuffer[(Int, Int)]): Unit = {
    val prevX: Int = lastX
    val prevY: Int = lastY
    val curX: Int = x
    val curY: Int = y
    val curTile: Byte = grid(y)(x)
    super.walk(grid)
    val newX: Int = x
    val newY: Int = y
    val dx: Int = newX - prevX
    val dy: Int = newY - prevY

    // West-East
    if (curTile == 5) {
      // Going east
      if (dx > 0) {
        setZone(path, zones, curX, curY - 1, 1)
        setZone(path, zones, curX, curY + 1, 2)

        // Going west
      } else {
        setZone(path, zones, curX, curY - 1, 2)
        setZone(path, zones, curX, curY + 1, 1)
      }

    // North-South
    } else if (curTile == 10) {
      // Going south
      if (dy > 0) {
        setZone(path, zones, curX+1, curY, 1)
        setZone(path, zones, curX-1, curY, 2)

      // Going north
      } else {
        setZone(path, zones, curX+1, curY, 2)
        setZone(path, zones, curX-1, curY, 1)
      }

    // Corner
    } else if (curTile != 15) {
      val east: Boolean = (curTile & 1) != 0
      val south: Boolean = (curTile & 2) != 0

      val toEast: Boolean = dx > 0
      val toSouth: Boolean = dy > 0

      val offsetBottom: Boolean = east ^ toEast ^ toSouth
      val zoneY: Boolean = !toSouth ^ east

      val offsetRight: Boolean = south ^ toEast ^ toSouth
      val zoneX: Boolean = toEast ^ south

      val offsetY: Int = if (offsetBottom) 1 else -1
      val zoneYVal: Int = if (zoneY) 2 else 1
      setZone(path, zones, curX, curY+offsetY, zoneYVal)

      val offsetX: Int = if (offsetRight) 1 else -1
      val zoneXVal: Int = if (zoneX) 2 else 1
      setZone(path, zones, curX+offsetX, curY, zoneXVal)
    }
  }

  private def setZone(path: ArrayBuffer[(Int, Int)], zones: Array[Array[Int]], posX: Int, posY: Int, zone: Int): Unit = {
    val height: Int = zones.length
    val width: Int = if (height == 0) 0 else zones(0).length
    if (0 <= posX && posX < width && 0 <= posY && posY < height) {
      if (!path.contains((posX, posY))) {
        zones(posY)(posX) = zone
      }
    }
  }
}
