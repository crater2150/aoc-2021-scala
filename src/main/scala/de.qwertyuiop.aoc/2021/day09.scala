package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*

import cats.*, cats.implicits.given

def day9(using InputSource): Unit =
  val heights = HeightMap(input(_.map(_ - '0').toVector).toVector)

  val lowPoints =
    for
      x <- 0 until heights.sizeX
      y <- 0 until heights.sizeY
      c = heights(x, y)
      if heights.neighbourValues(x, y).forall(_ > c)
    yield ((x, y), c)

  val riskLevel = lowPoints.map(_(1)).sum + lowPoints.size
  println(s"Risk level: $riskLevel")

  def findBasin(heights: HeightMap, start: (Int, Int)): Set[(Int, Int)] =
    val upwards = heights.neighbourCoords(start).filter(c => { val h = heights(c); h > heights(start) && h < 9 })
    (upwards.toSet) + start ++ upwards.flatMap(n => findBasin(heights, n))

  val largestBasinSizes = lowPoints.map((low, _) => findBasin(heights, low).size).sorted(summon[Ordering[Int]].reverse).take(3)
  println(s"Product of largest basins: ${largestBasinSizes.product}")


case class HeightMap(heights: Vector[Vector[Int]]):
  val sizeX = heights.size
  val sizeY = heights(0).size
  export heights.apply

  def apply(pos: (Int, Int)): Int = heights(pos(0))(pos(1))

  def neighbourValues(x: Int, y: Int): Vector[Int] =
    neighbourCoords((x, y)).map(apply)

  /* Can't reuse implementation from last year in Vec2D, as it also considers diagonals */
  def neighbourCoords(pos: (Int, Int)): Vector[(Int, Int)] =
    val (x,y) = pos
    val vb = collection.immutable.VectorBuilder[(Int, Int)]()
    if x > 0 then vb += ((x - 1, y))
    if x < sizeX - 1 then vb += ((x + 1, y))
    if y > 0 then vb += ((x, y - 1))
    if y < sizeY - 1 then vb += ((x, y + 1))
    vb.result()
