package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import Vectors.*
import Directions.*
import PaperFolding.*

def day13(using InputSource): Unit =
  val (dotsRaw, _ :: foldsRaw) = input().span(_.nonEmpty)
  val dots = dotsRaw.map(_.splitOnce(",").map((x,y) => Vec2D(x.toInt, y.toInt)).get).toSet
  val maxX = dots.map(_.x).max
  val maxY = dots.map(_.y).max
  val folds = foldsRaw.map {
    case s"fold along x=$x" => (true, x.toInt)
    case s"fold along y=$y" => (false, y.toInt)
  }

  val firstFold = foldDots(dots, folds.head(0), folds.head(1))
  println(s"\nPoints after first fold: ${firstFold.size}")

  val fullyFolded = folds.foldLeft(dots){ case (dots, (vert, pos)) => foldDots(dots, vert, pos) }
  printDots(fullyFolded)


object PaperFolding:
  def printDots(dots: Set[Vec2D[Int]]) =
    val maxX = dots.map(_.x).max
    val maxY = dots.map(_.y).max
    for y <- 0 to maxY do
      for x <- 0 to maxX do
        print(if dots.contains(Vec2D(x,y)) then '\u2588' else ' ')
      println()

  def foldDots(dots: Set[Vec2D[Int]], vertical: Boolean, position: Int) =
    val axis: Vec2D[Int] => Int = if vertical then _.x else _.y
    val dir: Dir = if vertical then West else South
    val (staying, moving) = dots.partition(d => axis(d) < position)
    val moved = moving.map(d => d.move(dir, 2 * math.abs(axis(d) - position)))
    staying ++ moved

