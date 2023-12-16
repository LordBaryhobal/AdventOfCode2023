package day16

object Mirrors extends Enumeration {
  type Mirror = Value
  val NONE = Value(".")
  val DIAGONAL_TL_BR = Value("\\")
  val DIAGONAL_BL_TR = Value("/")
  val SPLITTER_HOR = Value("-")
  val SPLITTER_VER = Value("|")
}
