package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

def day6(using InputSource): Unit =
  // list of staring fish ages
  val agesRaw = input(_.splitNN(",").map(_.toInt).toVector).head

  // Vector where agesStart(i) == number of fish with age i
  val agesStart = (Vector.iterate(0, 9)(_ + 1) ++ agesRaw).groupBy(identity).toVector.sorted.map(_._2.size.toLong - 1L)

  def step(ages: Vector[Long]): Vector[Long] =
    val next = (ages.tail :+ (ages.head)) // reduce timer for all fish, timer 0 => new fish with age 8
    next.updated(6, next(6) + ages.head)  // timer 0 => fish return to timer 6

  val after80 = (0 until 80).foldLeft(agesStart)((ages, _) => step(ages))
  val after256 = (80 until 256).foldLeft(after80)((ages, _) => step(ages))
  println(s"""Fish after 80 generations:  ${after80.sum}
             |Fish after 256 generations: ${after256.sum}""".stripMargin)
