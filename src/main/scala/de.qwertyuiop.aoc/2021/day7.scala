package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import util.chaining.given

def day7(using InputSource): Unit =
  val positions = input(_.splitNN(",").map(_.toInt).toVector).head

  def minCost(positions: Iterable[Int])(cost: (Int, Int) => Int) =
    (positions.min to positions.max)
      .map(target => (target, positions.map(start => cost(start, target)).sum))
      .minBy(_._2)

  minCost(positions)((s, t) => (s - t).abs).pipe(println)
  minCost(positions)((s, t) => { val diff = (s - t).abs; diff * (diff + 1) / 2 }).pipe(println)
