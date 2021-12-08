package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.{*, given}
import cats.*, cats.implicits.given
import util.chaining.given

def day8(using InputSource): Unit =
  val inputs = input(_.splitOnce(" \\| ") match
    case Some(possible, out) => (possible.splitNN(" "), out.splitNN(" "))
    case None => sys.error("invalid input")
  )
  SevenSeg.part1(inputs)
  SevenSeg.part2(inputs)

object SevenSeg:
  val uniqueLengths = Set(2,3,4,7)

  val digitForSegments = Map(
    "abcefg" -> 0,
    "cf" -> 1,
    "acdeg" -> 2,
    "acdfg" -> 3,
    "bcdf" -> 4,
    "abdfg" -> 5,
    "abdefg" -> 6,
    "acf" -> 7,
    "abcdefg" -> 8,
    "abcdfg" -> 9,
    )

  /*
   * 0 abc efg      aaaa
   * 1   c  f      b    c
   * 2 a cde g     b    c
   * 3 a cd fg      dddd
   * 4  bcd f      e    f
   * 5 ab d fg     e    f
   * 6 ab defg      gggg
   * 7 a c  f
   * 8 abcdefg
   * 9 abcd fg
   *   8687497 < number of occurrences
   */


  def part1(input: List[(List[String], List[String])]) =
    println("Part 1:")
    println(input.flatMap(_._2.map(_.length)).count(uniqueLengths.contains))

  def part2(input: List[(List[String], List[String])]) =
    println("Part 2:")
    println(input.map((digits, current) => decodeSingle(digits, current, false)).sum)

  def decodeSingle(allDigits: List[String], current: List[String], outputWirings: Boolean): Int =
    val byUniqueLength = allDigits.filter(uniqueLengths contains _.length).map(d => d.length -> d).toMap
    val charCounts = allDigits.flatten.countAll
    val b = charCounts.find(_._2 == 6).map(_._1).get
    val e = charCounts.find(_._2 == 4).map(_._1).get
    val f = charCounts.find(_._2 == 9).map(_._1).get
    val c = byUniqueLength(2).filter(_ != f).head
    val d = byUniqueLength(4).filterNot(Set(b, c, f).contains).head
    val a = byUniqueLength(3).filterNot(Set(c, f).contains).head
    val g = charCounts.find((char, count) => count == 7 && char != d).map(_._1).get
    val decoder = Map(a -> 'a', b -> 'b', c -> 'c', d -> 'd', e -> 'e', f -> 'f', g -> 'g')

    if outputWirings then
      println(s"""
       |  $a$a$a$a
       | $b    $c
       | $b    $c
       |  $d$d$d$d
       | $e    $f
       | $e    $f
       |  $g$g$g$g
       |""".stripMargin)

    current.map(_.map(decoder).sorted.pipe(digitForSegments)).mkString.toInt
