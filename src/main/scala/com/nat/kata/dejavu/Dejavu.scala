package com.nat.kata.dejavu

import scala.util.{Failure, Success, Try}

object Dejavu {

  def dejavu(input: ParsedParam, startPos: Int = 0): Int = {
    enumeratePattern(input)
      .groupBy[String](x => x)
      .size
  }

  def enumeratePattern(parsedInput: ParsedParam): List[String] = {
    val ParsedParam(_, requested, pattern) = parsedInput
    0.to(pattern.length - requested)
      .toList
      .map(_ => pattern)
      .zipWithIndex
      .map { tup =>
        val (pattern, index) = tup
        pattern.substring(index, index + requested)
      }
  }

  def parse(input: String): Either[String, ParsedParam] = {
    val invalidFormatString = "invalid parameter format:"
    input.split("\n").toList match {
      case line1 :: line2 :: Nil => parseLines(line2, line1, invalidFormatString)
      case Nil | _ :: Nil => Left(invalidFormatString + " accept 2 line if input")
    }
  }

  private def parseLines(line2: String, line1: String, invalidFormatString: String): Either[String, ParsedParam] = {
    line1.split(" ").toList match {
      case first :: second :: _ => buildParam(first, second, line2, invalidFormatString)
      case _ :: Nil => Left(invalidFormatString + " missing customer string length")
    }
  }

  private def buildParam(stock: String, requested: String, pattern: String, errorPrefix: String): Either[String, ParsedParam] = {
    Try(ParsedParam(stock.toInt, requested.toInt, pattern)) match {
      case Success(value) => verifyParam(errorPrefix, value)
      case Failure(exception) => Left(errorPrefix + " unable to parse to integer")
    }
  }

  private def verifyParam(errorPrefix: String, value: ParsedParam): Either[String, ParsedParam] = {
    val ParsedParam(length, expected, pattern) = value
    if (length < expected) Left(errorPrefix + " string is not suffice")
    else if (length != pattern.length) Left(errorPrefix + " stock length is not match pattern")
    else {
      Right(value)
    }
  }

  case class ParsedParam(stringLength: Int, customerExpected: Int, pattern: String)
}
