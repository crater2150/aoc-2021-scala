package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.{*, given}
import Vectors.*
import cats.*, cats.implicits.given
import ProbeStatus.*

def day17(using InputSource): Unit =
  given Ordering[Vec2D[Int]] = (summon[Ordering[(Int,Int)]].asInstanceOf[Ordering[Vec2D[Int]]])
  val s"target area: x=${xMinS}..${xMaxS}, y=${yMinS}..${yMaxS}" = input().head
  val (xMin, xMax, yMin, yMax)  = (xMinS.toInt, xMaxS.toInt, yMinS.toInt, yMaxS.toInt)
  val xRange = xMin to xMax
  val yRange = yMin to yMax
  val zero = Vec2D(0,0)

  def simulateSteps(pos: Vec2D[Int], xVelocity: Int, yVelocity: Int, visited: List[Vec2D[Int]]): List[Vec2D[Int]] =
    if pos.x > xMax || pos.y < yMin then pos :: visited
    else if xVelocity == 0 && !xRange.contains(pos.x) then pos:: visited
    else
      simulateSteps(
        pos + Vec2D(xVelocity, yVelocity),
        if xVelocity == 0 then 0 else xVelocity- xVelocity.sign,
        yVelocity - 1,
        pos :: visited
      )

  def minXSpeedRec(start: Int, sum: Int): Int =
    if sum < xMin then minXSpeedRec(start + 1, sum + start)
    else start -2
  val minXSpeed = minXSpeedRec(0,0)

  def checkSolution(steps: List[Vec2D[Int]]): ProbeStatus =
    val overshot :: candidate :: _ = steps
    if xRange.contains(candidate.x) && yRange.contains(candidate.y) then Hit
    else if overshot.x < xMin then TooNear
    else TooFar

  case class Flight(xSpeed: Int, ySpeed: Int, highest: Int)
  def highestY(xSpeed: Int) =
    val hits = LazyList.range(yMin, yMin + 1000).map(ySpeed => {
                val sim = simulateSteps(zero, xSpeed, ySpeed, List())
                (xSpeed, ySpeed, sim, checkSolution(sim))
              }).dropWhile(_._4 != Hit).take(1000).filter(_._4 == Hit)
    hits.map{ case (x,y,t,_) => Flight(x, y, y * (y + 1) / 2) }

  val flights = (
    for
      xSpeed <- minXSpeed to xMax + 10
      flight <- highestY(xSpeed).toList
    yield flight
  )
  val Flight(dxMax, dyMax, top) = flights.maxBy(_._3)
  println(s"Top height: $top")
  println(s"Number of valid shots: ${flights.size}")




enum ProbeStatus:
  case Hit
  case TooNear
  case TooFar
