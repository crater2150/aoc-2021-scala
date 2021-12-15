package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

def day14(using InputSource): Unit =
  val (template :: "" :: rulesRaw) = input()
  val rules = rulesRaw.flatMap(_.splitOnce(" -> ").map((pair, target) => pair -> (s"${pair.head}$target", s"$target${pair.tail}"))).toMap


  def solve(part: Int, steps: Int) =
    val occurrences = growPolymer(template, rules)(steps)
    val max = occurrences.maxBy(_._2)
    val min = occurrences.minBy(_._2)
    println(s"Most common:  ${max}")
    println(s"Least common: ${min}")
    println(s"Solution Part $part: ${max._2 - min._2}")

  solve(1,10)
  solve(2,40)

def growPolymer(start: String, rules: Map[String, (String, String)])(maxSteps: Int): Map[Char, Long] =
  def growRec(pairs: Map[String, Long])(steps: Int): Map[String, Long] =
    if steps == 0 then pairs
    else
      growRec(
        pairs.toVector.foldMap{(pair, count) => val (a, b) = rules(pair); Map(a -> count, b -> count) }
      )(steps - 1)

  val pairCounts = growRec(start.sliding(2).toVector.occurrences)(maxSteps)
    ++ Map(start.head.toString -> 1L, start.last.toString -> 1L)
  pairCounts.toVector.foldMap((pair, count) => Map(pair.head -> count))
