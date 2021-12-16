package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import Vectors.*

import cats.*, cats.implicits.given

def day9(using InputSource): Unit =
  val heights = HeightMap(input(_.map(_ - '0').toVector).toVector)

  val lowPoints =
    for
      x <- 0 until heights.sizeX
      y <- 0 until heights.sizeY
      c = heights(Vec2D(x, y))
      if heights.neighbourValues(x, y).forall(_ > c)
    yield (Vec2D(x, y), c)

  val riskLevel = lowPoints.map(_(1)).sum + lowPoints.size
  println(s"Risk level: $riskLevel")

  def findBasin(heights: HeightMap, start: Vec2D[Int]): Set[Vec2D[Int]] =
    val upwards = start.orthoNeighbours(heights.sizeX, heights.sizeY)
                       .filter(c => { val h = heights(c); h > heights(start) && h < 9 })
    (upwards.toSet) + start ++ upwards.flatMap(n => findBasin(heights, n))

  val largestBasinSizes = lowPoints.map((low, _) => findBasin(heights, low).size).sorted(summon[Ordering[Int]].reverse).take(3)
  println(s"Product of largest basins: ${largestBasinSizes.product}")


case class HeightMap(heights: Vector[Vector[Int]]):
  val sizeX = heights.size
  val sizeY = heights(0).size
  export heights.apply

  def apply(pos: Vec2D[Int]): Int = heights(pos.x)(pos.y)

  def neighbourValues(x: Int, y: Int): Vector[Int] =
    Vec2D(x, y).orthoNeighbours(sizeX, sizeY).map(apply)
