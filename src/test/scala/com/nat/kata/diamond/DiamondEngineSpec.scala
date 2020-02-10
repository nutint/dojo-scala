package com.nat.kata.diamond

import org.scalatest.{FreeSpec, Matchers}

class DiamondEngineSpec extends FreeSpec with Matchers {

  import DiamondEngine._

  "DiamondRow" - {

    "should work well when height = 1" in {
      Diamond(Height(1)).row(0) shouldBe "*"
    }

    "should work well when row height = 2" in {

      Diamond(Height(2)).row(0) shouldBe "*"
      Diamond(Height(2)).row(1) shouldBe "*"
    }

    "sandbox" - {

      "findTipSpaces" - {
        def isUpperRows(height: Int, row: Int): Boolean = true
        def findTipSpaces(height: Int): Int = (height + 1) / 2 - 1
        def findIntermediateSpaces(height: Int, row: Int): (Int, Int) = {
          val tipSpace = findTipSpaces(height)
          (0, 2)
        }
        "findTipSpace(3) shouldBe 1" in {
          /**
           *   x
           *  x x
           *   x
           */
          val height = 3

          findTipSpaces(height) shouldBe 1
          isUpperRows(height, 0) shouldBe true
          isUpperRows(height, 1) shouldBe true
          findIntermediateSpaces(height, 1) shouldBe (0, 1)
        }

        "findTipSpace(4) shouldBe 1" in {
          /**
           *   xx
           *  x  x
           *  x  x
           *   xx
           */
          findTipSpaces(4) shouldBe 1
          findIntermediateSpaces(4, 1) shouldBe (0,2)
          findIntermediateSpaces(4, 2) shouldBe (0,2)
        }

        "findTipSpace(5) shouldBe 2" in {
          /**
           *    x
           *   x x
           *  x   x
           *   x x
           *    x
           */
          findTipSpaces(5) shouldBe 2
        }

        "findTipSpace(6) shouldBe 2" in {
          /**
           *    xx
           *   x  x
           *  x    x
           *  x    x
           *   x  x
           *    xx
           */
          findTipSpaces(6) shouldBe 2
        }
      }

      "findIntermediateSpaces" - {
      }
    }

    "should work well when row height = 3" in {

      Diamond(Height(3)).row(0) shouldBe " *"
      Diamond(Height(3)).row(1) shouldBe "* *"
      Diamond(Height(3)).row(2) shouldBe " *"
    }
  }
}
