package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import Vectors.{*,given}
import util.chaining.given

extension (a: Int)
  def <->(b: Int): Range.Inclusive = (a min b) to (a max b)

case class Line(start: Vec2D[Int], end: Vec2D[Int]):
  def contains(p: Vec2D[Int]): Boolean =
    (start.x == end.x && start.x == p.x && (start.y <-> end.y).contains(p.y))
    ||
    (start.y == end.y && start.y == p.y && (start.x <-> end.x).contains(p.x))
    ||
    (start.x + start.y == end.x + end.y && p.x + p.y == end.x + end.y
      && (start.y <-> end.y).contains(p.y) && (start.x <-> end.x).contains(p.x))
    ||
    (p.x - p.y == end.x - end.y
      && (start.y <-> end.y).contains(p.y) && (start.x <-> end.x).contains(p.x))

def day5(using InputSource): Unit =
  val lines = input{ case s"$x1,$y1 -> $x2,$y2" => Line(Vec2D(x1.toInt,y1.toInt), Vec2D(x2.toInt,y2.toInt))}
  val orthogonalLines = lines.filter(l => l.start.x == l.end.x || l.start.y == l.end.y)
  val maxX = lines.flatMap(l => List(l.start.x, l.end.x)).max
  val maxY = lines.flatMap(l => List(l.start.y, l.end.y)).max
  println(dangerPoints(orthogonalLines, maxX, maxY))
  println(dangerPoints(lines, maxX, maxY))


def dangerPoints(lines: List[Line], maxX: Int, maxY: Int) = (
  for
    x <- 0 to maxX
    y <- 0 to maxY
  yield lines.count(_.contains(Vec2D(x,y)))
).count(_ > 1)

//debug output like in the description for part1
def printLines(lines: List[Line], maxX: Int, maxY: Int): Unit =
  for y <- 0 to maxY do
    for x <- 0 to maxX do
      print(lines.count(_.contains(Vec2D(x, y))).pipe(i => if i > 0 then i.toString else "."))
    println()

