package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.{*, given}
import cats.*, cats.implicits.given
import atto.*, Atto.*

def day16(using InputSource): Unit =
  val pkg = input().head
  println(BITSParser(pkg) | BITSParser.versionSum)
  println(BITSParser(pkg) | BITSParser.evaluate)

def day16test: Unit =
  import BITSParser.*
  val tests = List(
    ("C200B40A82", 3),
    ("04005AC33890", 54),
    ("880086C3E88112", 7),
    ("CE00C43D881120", 9),
    ("D8005AC2A8F0", 1),
    ("F600BC2D8F", 0),
    ("9C005AC2F8F0", 0),
    ("9C0141080250320F1802104A08",1 ),
  )
  tests.foreach((hex, expected) => {val pkg = BITSParser(hex); println((expected == evaluate(pkg), hex, expected, evaluate(pkg), show(pkg)))})

object BITSParser:

  def apply(hex: String): Payload = 
    val bin = hex.toVector.reverse.map(c => Integer.parseInt(c.toString,16).toBinaryString.padLeft(4,'0')).reverse.mkString
    pkg.parseOnly(bin).option.getOrElse(sys.error("Couldn't parse input"))

  final case class Header(version: Long, typ: Long)

  sealed trait Payload:
    def header: Header
  final case class Operator(header: Header, operands: List[Payload]) extends Payload
  final case class Literal(header: Header, value: Long) extends Payload


  val bit: Parser[Boolean] = (char('0') || char('1')).map(_.isRight)
  def bits(digits: Int) = take(digits).map(BigInt(_, 2).toLong)

  val header = (bits(3), bits(3)).mapN(Header.apply)

  val literal: Parser[Long] = (many(char('1') ~> take(4)), (char('0') ~> take(4)))
                  .mapN((init, last) => BigInt(init.mkString + last, 2).toLong)

  def literal(header: Header): Parser[Literal] = literal.map(Literal(header, _))

  val subpackageSized: Parser[List[Payload]] =
    for 
      len <- bits(15)
      payload <- take(len.toInt)
    yield many(pkg).parseOnly(payload).option.getOrElse(List())

  val subpackageCounted: Parser[List[Payload]] =
    for 
      len <- bits(11)
      payload <- manyN(len.toInt, pkg)
    yield payload

  def operator(header: Header) = 
    for
      usePackageCount <- bit
      payload <- if usePackageCount then subpackageCounted else subpackageSized
    yield Operator(header, payload)

  val pkg =
    for
      h      <- header
      result <- if h.typ == 4 then literal(h)
                else operator(h)
    yield result

  def versionSum(payload: Payload): Long = payload match
    case Operator(Header(vers,_), operands) => vers + operands.map(versionSum).sum
    case Literal(Header(vers,_),_) => vers

  extension (b: Boolean)
    def toLong: Long = if b then 1L else 0L

  def evaluate(payload: Payload): Long = payload match
    case Operator(Header(_, op), subPayload) =>
      val subValues = subPayload.map(evaluate)
      op match
        case 0 => subValues.sum
        case 1 => subValues.product
        case 2 => subValues.min
        case 3 => subValues.max
        case 5 => (subValues(0) > subValues(1)).toLong
        case 6 => (subValues(0) < subValues(1)).toLong
        case 7 => (subValues(0) == subValues(1)).toLong
        case u => sys.error(s"Invalid operator encountered: $u")
    case Literal(_, v) => v

  def show(payload: Payload): String = payload match
    case Operator(Header(_, op), subPayload) =>
      val subValues = subPayload.map(show)
      op match
        case 0 => s"sum($subValues)"
        case 1 => s"product($subValues)"
        case 2 => s"min($subValues)"
        case 3 => s"max($subValues)"
        case 5 => s"${subValues(0)} > ${subValues(1)}"
        case 6 => s"${subValues(0)} < ${subValues(1)}"
        case 7 => s"${subValues(0)} == ${subValues(1)}"
        case u => sys.error(s"Invalid operator encountered: $u")
    case Literal(_, v) => v.toString
