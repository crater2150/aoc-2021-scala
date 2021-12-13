package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

def day12(using InputSource): Unit =
  val edges = input(_.splitOnce("-").getOrElse(sys.error("Invalid input")))
    .flatMap((s,t) => List((s,t), (t,s)))
    .groupMapReduce(_(0))(e => Set(e(1)))(_ | _)

  def countPaths(edges: Map[String, Set[String]], allowSmallDoubleVisit: Boolean): Int=
    def rec(pathHead: String, visitedSmall: Set[String], canStillDoubleVisit: Boolean): Int=
      val neighbours = edges(pathHead).filter(n => n != "start" && (canStillDoubleVisit || !visitedSmall.contains(n)))
      neighbours.toVector.map(n =>
        if n == "end" then 1
        else if n.head.isLower then
          rec(n, visitedSmall + n, !visitedSmall.contains(n) && canStillDoubleVisit)
        else
          rec(n, visitedSmall, canStillDoubleVisit)
      ).sum
    rec("start", Set("start"), allowSmallDoubleVisit)

  val allPaths = countPaths(edges, false)
  val allPathsWithDoubleVisit = countPaths(edges, true)
  println(s"Found ${allPaths} paths with single visits on small caves only")
  println(s"Found ${allPathsWithDoubleVisit} paths with one double visit")
