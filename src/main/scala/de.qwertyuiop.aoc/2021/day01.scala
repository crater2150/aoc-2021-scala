package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

def day1(using InputSource): Unit =
  val numbers = input(_.toInt)
  println(countIncreases(numbers))
  println(countIncreasesSmoothed(numbers, 3))

/* part 1 */
def countIncreases(input: List[Int]): Int = input.sliding(2).count(l => l(0) < l(1))

/* part 2 */
def countIncreasesSmoothed(input: List[Int], smoothingWindow: Int) =
  countIncreases(input.sliding(smoothingWindow).map(_.sum).toList)

