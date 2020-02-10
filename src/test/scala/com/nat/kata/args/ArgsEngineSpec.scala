package com.nat.kata.args

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FreeSpec, Matchers}

class ArgsEngineSpec extends FreeSpec with Matchers {

  import ArgsEngine._

  "StringEngine" - {
    import StringEngine._

    "should parse all string in the following case" in {
      StringEngine.parse("abc") shouldBe Right(StringParsedResult("abc", ""))
    }

    "should left remaining when there is no double quote" in {
      val example =
        Table(
          ("wantedString", "remainingString", "expectedRemainingString"),
          ("abc", "d", "d"),
          ("abc", "d e", "d e"),
          ("abc", "d e  f g", "d e  f g")
        )

      forAll(example) { (wantedString, remainingString, expectedRemainingString) =>
        StringEngine.parse(s"$wantedString $remainingString") shouldBe Right(StringParsedResult(wantedString, expectedRemainingString))
      }
    }

    "should fail when start with quote but not have matched quote" in {
      StringEngine.parse("\"abc") shouldBe Left("string annotation does not match")
    }

    "should success when double quote is matched" in {
      StringEngine.parse("\"abc\"") shouldBe Right(StringParsedResult("abc", ""))
    }

    "should success and remaining left over when the quote is matched, and still have some to parse" in {
      val example =
        Table(
          ("exampleInput", "parsed", "leftOver"),
          ("\"abc\" def", "abc", " def"),
          ("\"abc\" \"def", "abc", " \"def" )
        )
      forAll(example) { (exampleInput, parsed, leftOver) =>
        StringEngine.parse(exampleInput) shouldBe Right(StringParsedResult(parsed, leftOver))
      }
    }


    "EscapedStringEngine" - {
      import StringEngine._

      "should change state after accept double quote" in {
        WaitingForInput.accept('\"') shouldBe ConsumeEscapedString("")
      }

      "should not change state after accept empty string" in {
        WaitingForInput.accept(' ') shouldBe WaitingForInput
      }

      "should fail when accept other scheme prefix (-)" in {
        WaitingForInput.accept('-') shouldBe ErrorConsuming("Missing input value")
      }

      "should accept value during consume string" in {
        val example =
          Table(
            ("acceptingValue", "consumedValue", "expectedValue"),
            ('a', "b", "ba"),
            ('c', "abc", "abcc")
          )

        forAll(example) { (acceptingValue, consumedValue, expectedValue) =>
          ConsumeEscapedString(consumedValue).accept(acceptingValue) shouldBe ConsumeEscapedString(expectedValue)
        }
      }

      "should done accepting value when accept another double quote" in {
        ConsumeEscapedString("abc").accept('\"') shouldBe DoneConsuming("abc")
      }

      "NonEscapedString" - {

        "should change state to ConsumeNonEscapedString after accept any char" in {
          WaitingForInput.accept('a') shouldBe ConsumeNonEscapedString("a")
        }

        "should append value after consume any char" in {
          ConsumeNonEscapedString("a").accept('b') shouldBe ConsumeNonEscapedString("ab")
        }

        "should change to done after consume space" in {
          ConsumeNonEscapedString("a").accept(' ') shouldBe DoneConsuming("a")
        }
      }
    }
  }

  "SchemeEngine" - {


  }

  "ArgsEngine" - {

    "ArgsEngineIdle" - {

      val schemes = List[ArgsScheme]()

      "should idle when accept space" in {
        ArgsEngineIdle(schemes).accept(' ') shouldBe ArgsEngineIdle(schemes)
      }

      "should be ArgsEngineShortScheme when accepts -" in {
        ArgsEngineIdle(schemes).accept('-') shouldBe ArgsEngineShortScheme(schemes)
      }

      "should be ArgsEngineTerminated when accepts others" in {
        ArgsEngineIdle(schemes).accept('a') shouldBe ArgsEngineTerminated(s"Unexpected char: a")
      }
    }

    "ArgsEngineShortScheme" - {

      import StringEngine._

      "should become ArgsEngineLongScheme when accepts another -" in {
        val schemes = List[ArgsScheme]()
        ArgsEngineShortScheme(schemes).accept('-') shouldBe ArgsEngineLongScheme(schemes, "")
      }

      "should detect non value scheme when the input scheme is matched with one of non valued scheme" in {
        val nonValuedScheme = NonValuedScheme('a', "aa", false)
        val foundNonValuedScheme = nonValuedScheme.markFound
        val schemes = List[ArgsScheme](nonValuedScheme)
        val modifiedScheme = List[ArgsScheme](foundNonValuedScheme)

        ArgsEngineShortScheme(schemes).accept('a') shouldBe ArgsEngineIdle(modifiedScheme)
      }

      "should detect value scheme when input scheme is matched with one of valued scheme" in {
        val valuedScheme = ValuedScheme('a', "aa", Nil)
        val schemes = List[ArgsScheme](valuedScheme)

        ArgsEngineShortScheme(schemes).accept('a') shouldBe ArgsEngineParseValue(schemes, valuedScheme, WaitingForInput)
      }
    }

    "ArgsEngineLongScheme" - {

      import StringEngine._

      "should parsed scheme when found a char" in {
        ArgsEngineLongScheme(Nil, "").accept('a') shouldBe ArgsEngineLongScheme(Nil, "a")
      }

      "should find matched scheme and found when find space, and there is a match scheme" in {
        val nonValuedScheme = NonValuedScheme('a', "aa", false)
        val foundNonValuedScheme = nonValuedScheme.markFound
        val schemes = List(nonValuedScheme)
        val modifiedScheme = List(foundNonValuedScheme)
        ArgsEngineLongScheme(schemes, "aa").accept(' ') shouldBe ArgsEngineIdle(modifiedScheme)
      }

      "should look for value when matched a ValuedScheme" in {
        val valuedScheme = ValuedScheme('a', "aa", Nil)
        val schemes = List(valuedScheme)
        ArgsEngineLongScheme(schemes, "aa").accept(' ') shouldBe ArgsEngineParseValue(schemes, valuedScheme, WaitingForInput)
      }
    }

    "ArgsEngineParseValue" - {

      import StringEngine._


      def assertArgsEngineToArgsEngine(srcStringEngine: EscapedStringEngine, event: Char, targetStringEngine: EscapedStringEngine): Any = {
        val valuedScheme = ValuedScheme('a', "aa", Nil)
        val schemes = List(valuedScheme)
        ArgsEngineParseValue(schemes, valuedScheme, srcStringEngine).accept(event) shouldBe
          ArgsEngineParseValue(schemes, valuedScheme, targetStringEngine)
      }

      "should be the same if accept empty string and the string engine is not finished yet" in {
        assertArgsEngineToArgsEngine(WaitingForInput, ' ', WaitingForInput)
      }

      "should error when accept `-` during waiting for input" in {
        val valuedScheme = ValuedScheme('a', "aa", Nil)
        ArgsEngineParseValue(List(valuedScheme), valuedScheme, WaitingForInput).accept('-') shouldBe
          ArgsEngineTerminated("Unable to parse value: Missing input value")
      }

      "ConsumeNonEscapedString" - {

        "should be the same if accept any char and the string engine is not finished yet" in {
          assertArgsEngineToArgsEngine(WaitingForInput, 'a', ConsumeNonEscapedString("a"))
        }

        "should add new value to current when accept another value" in {
          assertArgsEngineToArgsEngine(ConsumeNonEscapedString("a"), 'b', ConsumeNonEscapedString("ab"))
        }

        "should done consuming when accept space" in {
          val valuedScheme = ValuedScheme('a', "aa", Nil)
          val schemes = List(valuedScheme)
          ArgsEngineParseValue(schemes, valuedScheme, ConsumeNonEscapedString("ab")).accept(' ') shouldBe
            ArgsEngineIdle(List(valuedScheme.append("ab")))
        }

        "should become idle when mark as done" in {
          val valuedScheme = ValuedScheme('a', "aa", Nil)
          val schemes = List(valuedScheme)
          ArgsEngineParseValue(schemes, valuedScheme, ConsumeNonEscapedString("ab")).markDone shouldBe
            ArgsEngineIdle(List(valuedScheme.append("ab")))
        }
      }

      "ConsumeEscapedString" - {

        "should be the same if accept double quote and the string engine is not finished yet" in {
          assertArgsEngineToArgsEngine(WaitingForInput, '\"', ConsumeEscapedString(""))
        }

        "should add new value when accept space" in {
          assertArgsEngineToArgsEngine(ConsumeEscapedString(""), ' ', ConsumeEscapedString(" "))
        }

        "should add new value even if already have value" in {
          assertArgsEngineToArgsEngine(ConsumeEscapedString(" "), 'a', ConsumeEscapedString(" a"))
        }

        "should done consuming when accept double quote" in {
          val valuedScheme = ValuedScheme('a', "aa", Nil)
          ArgsEngineParseValue(List(valuedScheme), valuedScheme, ConsumeEscapedString(" a")).accept('\"') shouldBe
            ArgsEngineIdle(List(valuedScheme.append(" a")))
        }

      }

    }
  }

  "ArgsScheme" - {

    "NonValuedScheme" - {
      "should be found when call found" in {
        NonValuedScheme('a', "aa", false).markFound shouldBe NonValuedScheme('a', "aa", true)
      }

      "should return true when long scheme matched" in {
        NonValuedScheme('a', "aa", false).isLongSchemeMatched("aa") shouldBe true
      }

      "should return false when long scheme not matched" in {
        NonValuedScheme('a', "aab", false).isLongSchemeMatched("aa") shouldBe false
      }

      "should return true when short scheme matched" in {
        NonValuedScheme('a', "aab", false).isShortSchemeMatched('a') shouldBe true
      }

      "should return false when short scheme not matched" in {
        NonValuedScheme('a', "aab", false).isShortSchemeMatched('b') shouldBe false
      }
    }

    "ValuedScheme" - {

      "should append value when call append" in {
        ValuedScheme('a', "aa", Nil).append("abc") shouldBe ValuedScheme('a', "aa", List("abc"))
      }

      "should return true when long scheme matched" in {
        ValuedScheme('a', "aa", Nil).isLongSchemeMatched("aa") shouldBe true
      }

      "should return false when long scheme not matched" in {
        ValuedScheme('a', "aab", Nil).isLongSchemeMatched("aa") shouldBe false
      }

      "should return true when short scheme matched" in {
        ValuedScheme('a', "aab", Nil).isShortSchemeMatched('a') shouldBe true
      }

      "should return false when short scheme not matched" in {
        ValuedScheme('a', "aab", Nil).isShortSchemeMatched('b') shouldBe false
      }
    }
  }

  "parseArguments" - {

    "Logging Port Output Scenario" - {

      val schemes = List(
        NonValuedScheme('l', "logging", false),
        ValuedScheme('p', "port", Nil),
        ValuedScheme('o', "output", Nil)
      )

      "should parse correct with the following input `-l -p 3000 -o abc`" in {
        parseArguments("-l -p 3000 -o abc", schemes) shouldBe Right(List(
          NonValuedScheme('l', "logging", false).markFound,
          ValuedScheme('p', "port", Nil).append("3000"),
          ValuedScheme('o', "output", Nil).append("abc")))
      }

      "should parse correct with the following input `--logging --port 3000 --output abc`" in {
        parseArguments("--logging --port 3000 --output abc", schemes) shouldBe Right(List(
          NonValuedScheme('l', "logging", false).markFound,
          ValuedScheme('p', "port", Nil).append("3000"),
          ValuedScheme('o', "output", Nil).append("abc")))
      }

      "should work well when passing escaped string as an input" in {
        parseArguments("--logging --port \"a 3000\" --output \"abc    \"", schemes) shouldBe Right(List(
          NonValuedScheme('l', "logging", false).markFound,
          ValuedScheme('p', "port", Nil).append("a 3000"),
          ValuedScheme('o', "output", Nil).append("abc    ")))
      }

      "should work in any order" in {
        parseArguments("-p 3000 -o abc -l", schemes) shouldBe Right(List(
          NonValuedScheme('l', "logging", false).markFound,
          ValuedScheme('p', "port", Nil).append("3000"),
          ValuedScheme('o', "output", Nil).append("abc")))
      }

      "should work when there is spaces after the arguments" in {
        parseArguments("-p 3000 -o abc -l   ", schemes) shouldBe Right(List(
          NonValuedScheme('l', "logging", false).markFound,
          ValuedScheme('p', "port", Nil).append("3000"),
          ValuedScheme('o', "output", Nil).append("abc")))
      }

      "when there is no arguments, the expected scheme should be default" in {
        parseArguments("", schemes) shouldBe Right(schemes)
      }
    }
  }
}
