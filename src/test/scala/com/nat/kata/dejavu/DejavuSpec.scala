package com.nat.kata.dejavu

import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks._

class DejavuSpec extends FreeSpec with Matchers {

  import Dejavu._

  "should pass" in {
    true shouldBe true
  }

  "parsing input" - {

    "should fail(accept 2 line if input) when input only 1 line of input" in {
      parse("1 line of input") shouldBe Left("invalid parameter format: accept 2 line if input")
    }

    "should fail(missing customer string length) when parameter line 1 is not exceed 2 element" in {
      parse("1\n2") shouldBe Left("invalid parameter format: missing customer string length")
    }

    "should fail when there is only 1 line" in {
      parse("1 2\n") shouldBe Left("invalid parameter format: accept 2 line if input")
    }

    "should pass success when there are enoughtInput input" in {
      parse("3 1\n010") shouldBe Right(ParsedParam(3, 1, "010"))
    }

    "should fail when any of first line is not string" in {
      val example =
        Table(
          ("firstInput", "secondInput"),
          ("1", "anyString"),
          ("anyString", "3"),
          ("anyString", "anyString")
        )

      forAll(example) { (firstInput, secondInput) =>
        parse(s"$firstInput, $secondInput\n101010") shouldBe Left("invalid parameter format: unable to parse to integer")
      }
    }

    "should fail when number of current string length is lower then customer string length request" in {
      val stockStringLength = 5
      val requestedStringLength = 8
      parse(s"$stockStringLength $requestedStringLength\n10101") shouldBe Left("invalid parameter format: string is not suffice")
    }

    "should fail when pattern's length is not match stocked string length" in {
      val example = Table(
        ("stockLength", "pattern"),
        ("1", "123"),
        ("2", "1"),
        ("3", "1234")
      )

      forAll(example) { (stockLength, pattern) =>
        parse(s"$stockLength 1\n$pattern") shouldBe Left("invalid parameter format: stock length is not match pattern")
      }
    }

  }

  "dejavu" - {

    "input 5 2, and 01101 should output 3" in {
      dejavu(ParsedParam(5, 2, "01101")) shouldBe 3
    }

    "input 10 3, and 1110001011 should output 8" in {
      dejavu(ParsedParam(10, 3, "1110001011")) shouldBe 8
    }

    "input 10 3, and 11100010111 should output 8" in {
      dejavu(ParsedParam(11, 3, "11100010111")) shouldBe 8
    }

    "input 10 3, and 11100010111000 should output 8" in {
      dejavu(ParsedParam(11, 3, "111000101110000")) shouldBe 8
    }

  }

  "enumeratePattern" - {

    "should enumerate correctly" in {
      enumeratePattern(ParsedParam(2, 1, "01")) shouldBe List("0", "1")
      enumeratePattern(ParsedParam(3, 1, "011")) shouldBe List("0", "1", "1")
      enumeratePattern(ParsedParam(3, 2, "011")) shouldBe List("01", "11")
      enumeratePattern(ParsedParam(4, 2, "0110")) shouldBe List("01", "11", "10")
      enumeratePattern(ParsedParam(10, 3, "1110001011")) shouldBe List("111", "110", "100", "000", "001", "010", "101", "011")
      enumeratePattern(ParsedParam(11, 3, "11100010111")) shouldBe List("111", "110", "100", "000", "001", "010", "101", "011", "111")
    }
  }

  
}