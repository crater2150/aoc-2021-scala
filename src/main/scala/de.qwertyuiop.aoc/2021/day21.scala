package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

final case class DiceRoll(sum: Long, first: Long, second: Long, third: Long):
  def show: String = s"$first+$second+$third"

final case class GameResult(losingScore: Long, rolls: Long, losingPlayer: Long):
  def show: String = s"Player $losingPlayer lost after $rolls rolls, with score $losingScore (rating: ${rolls * losingScore})"

def day21(using InputSource): Unit =
  val List(p1, p2) = input{ case s"Player $p starting position: $i" => i.toLong }
  val detDice = LazyList.from(0).map(_ % 100 + 1).sliding(3,3)
                        .map{case LazyList(a,b,c) => DiceRoll(a + b + c, a, b, c)}.to(LazyList)


  def move(pos: Long, amount: Long) = 1 + (pos + amount - 1) % 10

  def rollUntilWon(winningPoints: Long)(
    p1points: Long, p2points: Long, p1pos: Long, p2pos: Long, dice: LazyList[DiceRoll], rounds: Long
  ): GameResult =
    val curPlayer = if rounds % 2 == 0 then 1 else 2
    if p2points >= winningPoints then GameResult(p1points, rounds * 3, curPlayer)
    else
      val roll = dice.head
      val newP1Pos = move(p1pos, roll.sum)

      //println(s"Player $curPlayer rolls ${roll.show} and moves from ${p1pos} to space $newP1Pos for a total score of ${p1points + newP1Pos}")

      rollUntilWon(winningPoints)(p2points, p1points + newP1Pos, p2pos, newP1Pos, dice.tail, rounds + 1)

  val rolls3D3 = (for a <- 1 to 3; b <- 1 to 3; c <- 1 to 3 yield a+b+c).occurrences.toVector
  def countWins(winningPoints: Long)(startP1: Long, startP2: Long): (Long, Long) = 
    lazy val mRecurse = memoize((recurse _).tupled)
    def recurse(p1points: Long, p2points: Long, p1pos: Long, p2pos: Long, rounds: Long): (Long, Long) =
      if p2points >= winningPoints then (1 - rounds % 2, rounds % 2) // current player gets a point
      else
        val newP1Pos = (p1pos) % 10 + 1
        
        rolls3D3.map{(roll, freq) => 
            val newPos = move(p1pos, roll)
            val (x,y) = mRecurse((p2points, p1points + newPos, p2pos, newPos, rounds + 1))
            (x * freq, y * freq)
        }.combineAll
    mRecurse(0, 0, startP1, startP2, 0)

  val trainingRound = rollUntilWon(1000)(0, 0, p1, p2, detDice, 0)
  val (p1wins,p2wins) = countWins(21)(p1, p2)

  println(s"Player 1 starts at $p1\nPlayer 2 starts at $p2")
  println(trainingRound.show)
  println("Switching to dirac dice")
  println(s"Player 1 wins in $p1wins universes")
  println(s"Player 2 wins in $p2wins universes")
