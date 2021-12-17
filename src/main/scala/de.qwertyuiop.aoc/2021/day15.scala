package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import Vectors.{*, given}
import cats.*, cats.implicits.given
import util.chaining.given
import Day15._

def day15(using InputSource): Unit =
  val dangerZones = InputFormats.intMatrix
  val fullDangerZones = fullMap(dangerZones)


  println(s"Way through tile: ${findChitonWay(dangerZones).get._2}")
  println(s"Way through all tiles: ${findChitonWay(fullDangerZones).get._2}")


object Day15:
  def inc(line: Vector[Int]) = line.map(i => ((i + 1) % 10) max 1)
  def incNest(tile: Vector[Vector[Int]]) = tile.map(inc)

  def fullMap(tile: Vector[Vector[Int]]): Vector[Vector[Int]] =
    val topRow = tile.map(l => (1 to 4).foldLeft(l, l){ case ((line, prev), _) => val next = inc(prev); (line ++ next, next) }._1)
    (1 to 4).foldLeft(topRow, topRow){ case ((line, prev), _) => val next = incNest(prev); (line ++ next, next) }._1

  def findChitonWay(unpaddedDangerZones: Vector[Vector[Int]]) = 
    val dangerZones = pad(999, unpaddedDangerZones)
    val (maxX, maxY) = (dangerZones.size, dangerZones(0).size)
    dijkstra[Vec2D[Int], Long](Vec2D(1,1), 0L, Vec2D.allCoords(dangerZones).toSet, Vec2D(maxX - 2, maxY - 2), Long.MaxValue)(
      neighbours = _.orthoNeighbours(maxX, maxY).toSet,
      calcCost = (_,t) => dangerZones.get(t)
    )

  def pad[T](padding: T, matrix: Vector[Vector[T]]) =
    val hor = Vector.fill(matrix(0).size + 2)(padding)
    hor +: matrix.map(padding +: _ :+ padding) :+ hor
