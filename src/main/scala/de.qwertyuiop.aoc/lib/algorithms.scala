package de.qwertyuiop.aoc.lib
import Vectors.*
import math.Numeric.Implicits.infixNumericOps
import math.Ordering.Implicits.infixOrderingOps
import cats.*, cats.implicits.given

def dijkstra[T, C](start: T, startCost: C, allNodes: Set[T], target: T, maxValue: C)
(neighbours: T => Set[T], calcCost: (T,T) => C)
(using n: Numeric[C], eq: CanEqual[T,T]): Option[(List[T], C)] =
  def rec(unvisited: Set[T], dists: Map[T, C], prevs: Map[T, T]): Option[(List[T], C)] =
    if unvisited.size % 1000 == 0 then println(s"To visit: ${unvisited.size}")
    val current = (dists.keySet & unvisited).minBy(t => dists.getOrElse(t, maxValue))
    if current == target then (backtrack(target, start, prevs, dists), dists(target)).some
    else
      val cost = dists(current)
      val (changedDists, changedPrevs) = (
        for
          neigh <- neighbours(current) & unvisited
          newCost = cost + calcCost(current, neigh)
          if newCost < dists.getOrElse(neigh, maxValue)
        yield (neigh -> newCost, neigh -> current)
      ).unzip
      val newDists = dists ++ changedDists
      val newPrevs = prevs ++ changedPrevs
      val unvisitedNow = unvisited - current
      if unvisitedNow.isEmpty then None
      else rec(unvisitedNow, newDists, newPrevs)
      
  
  def backtrack(target: T, start: T, prevs: Map[T,T], dists: Map[T,C]): List[T] =
    def btRec(current: T, path: List[T]): List[T] =
      if current == start then current :: path
      else btRec(prevs(current), current :: path)
    btRec(target, Nil)

  rec(allNodes, Map(start -> startCost), Map())


