package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import util.chaining.given

type Card = Vector[Vector[Int]]

def day4(using InputSource): Unit =
  val drawsRaw #:: cardsRaw = input().split("")
  val draws = drawsRaw.head.splitNN(",").map(_.toInt)
  val cards = cardsRaw.map(_.map(_.trim.nn.splitNN("\\s+").map(_.toInt).toVector).toVector).toSet


  def cardHasWon(c: Card, drawn: List[Int]): Boolean =
        c.exists(_.forall(drawn.contains)) || c.transpose.exists(_.forall(drawn.contains))

  def drawUntilWon(drawn: List[Int], remaining: List[Int], cards: Set[Card]): Option[(List[Int], Card)] =
    cards.find(c => cardHasWon(c, drawn)) match
      case Some(winner) => (drawn, winner).some
      case None => remaining match
        case next :: rest => drawUntilWon(next :: drawn, rest, cards)
        case Nil => None

  def findLastWinner(drawn: List[Int], remaining: List[Int], cards: Set[Card]): Option[(List[Int], Card)] =
    val notWonYet = cards.filterNot(c => cardHasWon(c, drawn))
    if notWonYet.size == 1 then
      drawUntilWon(drawn, remaining, notWonYet)
    else
      remaining match
        case next :: rest => findLastWinner(next :: drawn, rest, notWonYet)
        case Nil => None

  def calculatePoints(drawn: List[Int], winner: Card): Int =
    winner.flatten.filterNot(drawn.contains).sum * drawn.head


  val winningScore = drawUntilWon(List(draws.head), draws.tail, cards).map(_.pipe(calculatePoints).toString).getOrElse("no Winner")
  val losingScore = findLastWinner(List(draws.head), draws.tail, cards).map(_.pipe(calculatePoints).toString).getOrElse("no Winner")
  println(s"Winning card score: $winningScore")
  println(s"Losing card score: $losingScore")
