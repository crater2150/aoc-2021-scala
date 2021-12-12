package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

import Vectors.{*, given}

def day11(using InputSource): Unit =
  import Octopodes.*
  val fieldRaw = inputF(_.map(_ - '0').toVector)(Vector)
  val field = (
    for
      x <- 0 until fieldRaw.size
      y <- 0 until fieldRaw(0).size
    yield Vec2D(x,y) -> fieldRaw(x)(y)
  ).toMap

  val r = render(fieldRaw.size, fieldRaw(0).size)


  val (finalField, finalFlashes) =
    (1 to 100).foldLeft((field, 0)){ case ((field, flashes), _) =>
      val (newField, newFlashes) = energize(field)
      (newField, flashes + newFlashes)
    }
  println(s"After $finalFlashes flashes:")
  r(finalField)

  val inSync = findSync(field, 0)
  println(s"In sync after $inSync steps")


object Octopodes:
  def render(xSize: Int, ySize: Int)(field: Map[Vec2D[Int], Int]) =
      for x <- 0 until xSize do
        for y <- 0 until ySize do
          print(field(Vec2D(x,y)).toHexString)
        println()

  def energize(f: Map[Vec2D[Int], Int]): (Map[Vec2D[Int], Int], Int) =
    @annotation.tailrec
    def flashOctopodes(f: Map[Vec2D[Int], Int], flashed: Set[Vec2D[Int]]): (Map[Vec2D[Int], Int], Int) =
      val flashing = f.filter((coord, v) => v > 9).keySet &~ flashed
      if flashing.isEmpty then
        (f.view.mapValues(v => if v > 9 then 0 else v).toMap, flashed.size + flashing.size)
      else
        val neighs = flashing.toVector.flatMap(_.neighbours).occurrences
        val newF = f.map((coord, v) => coord -> (
            if v > 9 || flashing.contains(coord) then 12
            else (v + neighs.getOrElse(coord, 0)).min(11)
          ))
        flashOctopodes(newF, flashed | flashing)
    flashOctopodes(f.mapValuesS(_ + 1), Set.empty)

  def findSync(f: Map[Vec2D[Int], Int], step: Int): Int =
    val (newField, newFlashes) = energize(f)
    if newFlashes == f.size then step + 1
    else findSync(newField, step + 1)
