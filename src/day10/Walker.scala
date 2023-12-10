package day10

class Walker {
  protected var x: Int = 0
  protected var y: Int = 0
  protected var lastX: Int = -1
  protected var lastY: Int = -1
  protected var distance: Int = 0

  def this(startX: Int, startY: Int) = {
    this()
    x = startX
    y = startY
  }

  def this(startX: Int, startY: Int, excludeX: Int, excludeY: Int) = {
    this(startX, startY)
    lastX = excludeX
    lastY = excludeY
  }

  def getX(): Int = x
  def getY(): Int = y
  def getPos(): (Int, Int) = (x, y)
  def getDistance(): Int = distance

  def walk(grid: Array[Array[Byte]]): Unit = {
    val height: Int = grid.length
    val width: Int = if (height == 0) 0 else grid(0).length
    val (x2: Int, y2: Int) = getNextPos(grid, width, height)
    lastX = x
    lastY = y
    x = x2
    y = y2
    distance += 1
  }

  private def getNextPos(grid: Array[Array[Byte]], width: Int, height: Int): (Int, Int) = {
    val curTile: Byte = grid(y)(x)
    for (((dx: Int, dy: Int), i: Int) <- Walker.OFFSETS.zipWithIndex) {
      val x2: Int = x + dx
      val y2: Int = y + dy
      if (x2 != lastX || y2 != lastY) {
        if (0 <= x2 && x2 < width && 0 <= y2 && y2 < height) {
          val bit: Byte = (1 << i).toByte
          val bit2: Byte = (1 << ((i + 2) % 4)).toByte
          if ((curTile & bit) != 0 && (grid(y2)(x2) & bit2) != 0) {
            return (x2, y2)
          }
        }
      }
    }

    throw new Exception("Dead-end path")
  }
}

object Walker {
  val OFFSETS: Array[(Int, Int)] = Array(
    (1, 0), (0, 1), (-1, 0), (0, -1)
  )
}