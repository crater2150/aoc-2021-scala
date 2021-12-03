package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import java.lang.Integer.parseInt

def day3(using InputSource): Unit =
  val reportRaw = input()

  // part 1
  val onesPerDigit = countBits(reportRaw)
  val gamma = decode(onesPerDigit, _ > reportRaw.size / 2)
  val epsilon = decode(onesPerDigit, _ < reportRaw.size / 2)

  println(s"""
    γ: $gamma
    ε: $epsilon
    Part 1 solution: ${gamma * epsilon}
    """)

  // part 2
  val (o2, co2) = lifeSupportRating(reportRaw)
  val (o2dec, co2dec) = (parseInt(o2, 2), parseInt(co2, 2))

  println(s"""
    O2: $o2dec ($o2)
    CO2: $co2dec ($co2)
    Part 2 solution: ${o2dec * co2dec}
    """)


def countBits(bitWords: Iterable[String]): Vector[Int] =
  bitWords.foldLeft(Vector.fill(bitWords.head.size)(0)) { (sum, word) =>
    sum.zip(word.map(_ - '0').toVector).map(t => t._1 + t._2)
  }

def decode(bitCounts: Vector[Int], condition: Int => Boolean) =
  parseInt(bitCounts.map(i => if condition(i) then '1' else '0').mkString, 2)


def lifeSupportRating(bitWords: List[String]): (String, String) =
  def filterOut(remaining: List[String], position: Int, keep: (Int, Int) => Boolean): String =
    remaining match
      case Nil => sys.error("Invalid input, life support rating not possible")
      case last :: Nil => last
      case rest =>
        val byBit = rest.groupBy(_(position))
        def amount[K](m: Map[K, List[_]], key: K): Int = m.get(key).map(_.size).getOrElse(0)
        filterOut(
          if keep(amount(byBit, '1'), amount(byBit, '0')) then byBit('1') else byBit('0'),
          position + 1, keep)

  (filterOut(bitWords, 0, _ >= _), filterOut(bitWords, 0, _ < _))

