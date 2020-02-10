package com.nat.kata.args

import com.nat.kata.args.ArgsEngine.StringEngine.{ConsumeNonEscapedString, DoneConsuming, WaitingForInput, WorkingStringEngine}

trait ArgsEngine

object ArgsEngine {

  def parseArguments(argumentString: String, schemes: List[ArgsScheme]): Either[String, List[ArgsScheme]] =
    argumentString.foldLeft[ArgsEngine](ArgsEngineIdle(schemes)) { (engine, c) =>
      engine match {
        case workingArgsEngine: WorkingArgsEngine => workingArgsEngine.accept(c)
        case a => a
      }
    } match {
      case ArgsEngineIdle(schemes) => Right(schemes)
      case pv: ArgsEngineParseValue => Right(pv.markDone.schemes)
      case x => Left(s"something went wrong $x")
    }

  object StringEngine {
    def parse(input: String): Either[String, StringParsedResult] =
      parseEscapedString(input)

    private def parseEscapedString(input: String): Either[String, StringParsedResult] = {
      val (escapedStringEngine: EscapedStringEngine, leftOver: String) =
      input.toCharArray.foldLeft[(EscapedStringEngine, String)]((WaitingForInput, ""))
        { (engineAndLeftOver: (EscapedStringEngine, String), c: Char) =>
          val (engine, leftOver) = engineAndLeftOver
          engine match {
            case WaitingForInput => (WaitingForInput.accept(c), "")
            case consumed: ConsumeEscapedString => (consumed.accept(c), "")
            case consumed: ConsumeNonEscapedString => (consumed.accept(c), "")
            case d: DoneConsuming => (d, leftOver + c)
          }
        }
      escapedStringEngine match {
        case WaitingForInput => Left("string annotation does not match")
        case ConsumeEscapedString(_) => Left("string annotation does not match")
        case ConsumeNonEscapedString(consumed) => Right(StringParsedResult(consumed, ""))
        case DoneConsuming(consumed) => Right(StringParsedResult(consumed, leftOver))
      }
    }


    sealed trait EscapedStringEngine
    sealed trait WorkingStringEngine extends EscapedStringEngine {
      def accept(c: Char): EscapedStringEngine
    }
    case object WaitingForInput extends WorkingStringEngine {
      def accept(c: Char): EscapedStringEngine = c match {
        case '\"' => ConsumeEscapedString("")
        case ' ' => WaitingForInput
        case '-' => ErrorConsuming("Missing input value")
        case a => ConsumeNonEscapedString(s"$a")
      }
    }
    case class ConsumeEscapedString(parsedString: String) extends WorkingStringEngine {
      def accept(c: Char): EscapedStringEngine = c match {
        case '\"' => DoneConsuming(parsedString)
        case _ => ConsumeEscapedString(parsedString + c)
      }
    }
    case class ConsumeNonEscapedString(parsedString: String) extends WorkingStringEngine {
      def accept(c: Char): EscapedStringEngine = c match {
        case ' ' => DoneConsuming(parsedString)
        case a => ConsumeNonEscapedString(parsedString + a)
      }
    }
    case class DoneConsuming(str: String) extends EscapedStringEngine

    case class ErrorConsuming(str: String) extends EscapedStringEngine

    case class StringParsedResult(parsed: String, remaining: String)
  }


  sealed trait WorkingArgsEngine extends ArgsEngine {
    def accept(c: Char): ArgsEngine
  }
  case class ArgsEngineIdle(schemes: List[ArgsScheme]) extends WorkingArgsEngine {
    def accept(c: Char): ArgsEngine = c match {
      case ' ' => this
      case '-' => ArgsEngineShortScheme(schemes)
      case x => ArgsEngineTerminated(s"Unexpected char: $x")
    }
  }

  case class ArgsEngineShortScheme(schemes: List[ArgsScheme]) extends WorkingArgsEngine {
    def accept(c: Char): ArgsEngine = c match {
      case '-' => ArgsEngineLongScheme(schemes, "")
      case a if schemes.exists(as => isShortAndNonValuedScheme(a, as)) =>
        ArgsEngineIdle(
          schemes.map {
            case n@NonValuedScheme(nc, _, _) if c == nc => n.markFound
            case v@ValuedScheme(vc, _, _) if c == vc => v
            case x => x
          }
        )
      case a if schemes.exists(as => as.isShortSchemeMatched(a) && as.isInstanceOf[ValuedScheme]) =>
        ArgsEngineParseValue(
          schemes,
          schemes.find(as => as.isShortSchemeMatched(a) && as.isInstanceOf[ValuedScheme]).get.asInstanceOf[ValuedScheme],
          WaitingForInput)
    }

    private def isShortAndNonValuedScheme(shortScheme: Char, argsScheme: ArgsScheme) = {
      argsScheme.isShortSchemeMatched(shortScheme) && argsScheme.isInstanceOf[NonValuedScheme]
    }
  }
  case class ArgsEngineLongScheme(schemes: List[ArgsScheme], parsedScheme: String) extends WorkingArgsEngine {
    def accept(c: Char): ArgsEngine = c match {
      case ' ' if schemes.exists(isMatchedValueScheme) => toParseValue
      case ' ' => ArgsEngineIdle(
        schemes.map {
          case n@NonValuedScheme(_, nl, _) if parsedScheme == nl => n.markFound
          case x => x
        }
      )
      case a => copy(parsedScheme = parsedScheme + a)
    }

    def isMatchedValueScheme(s: ArgsScheme): Boolean = s.isLongSchemeMatched(parsedScheme) && s.isInstanceOf[ValuedScheme]

    private def toParseValue: ArgsEngineParseValue =
      ArgsEngineParseValue(
        schemes,
        schemes.find(isMatchedValueScheme).get.asInstanceOf[ValuedScheme],
        WaitingForInput)

  }

  case class ArgsEngineParseValue(schemes: List[ArgsScheme], activeScheme: ValuedScheme, stringEngine: StringEngine.EscapedStringEngine) extends WorkingArgsEngine {

    import StringEngine._

    def accept(c: Char): ArgsEngine = stringEngine match {
      case w: WorkingStringEngine => w.accept(c) match {
        case DoneConsuming(str) => markDone(str)
        case StringEngine.ErrorConsuming(str) => ArgsEngineTerminated(s"Unable to parse value: $str")
        case aw: WorkingStringEngine => copy(stringEngine = aw)
      }
      case DoneConsuming(str) => ???
    }

    def markDone: ArgsEngineIdle = {
      stringEngine match {
        case ConsumeNonEscapedString(parsed) =>markDone(parsed)
        case ConsumeEscapedString(parsed) => markDone(parsed)
        case _ => markDone("")
      }
    }

    def markDone(value: String): ArgsEngineIdle = ArgsEngineIdle(schemes.map{
      case `activeScheme` => activeScheme.append(value)
      case a => a
    })
  }

  case class ArgsEngineTerminated(reason: String) extends ArgsEngine

  sealed trait ArgsScheme {
    def shortScheme: Char
    def longScheme: String
    def isLongSchemeMatched(inputLongScheme: String): Boolean = inputLongScheme == longScheme
    def isShortSchemeMatched(c: Char): Boolean = shortScheme == c
  }
  case class NonValuedScheme(shortScheme: Char, longScheme: String, found: Boolean) extends ArgsScheme {
    def markFound: NonValuedScheme = copy(found = true)
  }
  case class ValuedScheme(shortScheme: Char, longScheme: String, values: List[String]) extends ArgsScheme {
    def append(value: String): ValuedScheme = copy(values = value :: values)
  }
}
