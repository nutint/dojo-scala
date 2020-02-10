package com.nat.kata.diamond

object DiamondEngine {

  case class Diamond(height: Height) {
    private def findTipSpaces(height: Int): Int = (height + 1) / 2 - 1

    def row(rowNum: Int): String = {
      if(rowNum == 0 || rowNum == height.value - 1) {
        val prefixSpaces = findTipSpaces(height.value)
        (s" " * prefixSpaces ) + "*"
      } else {
        "**"
      }

    }
  }
  case class Height(value: Int)
}
