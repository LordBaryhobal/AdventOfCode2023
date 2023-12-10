package util

object Ansi {
  val ESC: String = "\u001b["
  val CLEAR: String = code("0")
  val BOLD: String = code("1")
  val FAINT: String = code("2")
  val ITALIC: String = code("3")
  val UNDERLINE: String = code("4")
  val SLOW_BLINK: String = code("5")
  val RAPID_BLINK: String = code("6")
  val REVERSE: String = code("7")
  val CONCEAL: String = code("8")
  val STRIKETHROUGH: String = code("9")
  def FG_RGB(r: Int, g: Int, b: Int): String = code(s"38;2;$r;$g;${b}")
  def BG_RGB(r: Int, g: Int, b: Int): String = code(s"48;2;$r;$g;${b}")

  private def code(str: String): String = ESC+str+"m"
}