package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import atto.*, Atto.*

def day18(using InputSource): Unit =
  import Snailfish.*
  val lines = input()
  val snumbers = lines.map(i => snailfishNumber.parseOnly(i).option.get)
  val result = snumbers.tail.foldLeft(snumbers.head)(_ + _)
  //val sum = Pair(snumbers(0), snumbers(1))
  //tryExplode(sum)
  //println(reduce(snailfishNumber.parseOnly("[[[[0,7],4],[7,[[8,4],9]]],[1,1]]").option.get))
  println(s"Reduced:   $result")
  println(s"Magnitude: ${result.magnitude}")

  val maxMag = snumbers.combinations(2).map{ case List(a, b) => (a + b).magnitude max (b + a).magnitude }.max
  println(s"maximum:   $maxMag")


enum Dir:
  case L, R
extension (d: Dir)
  def opposite: Dir = if d == Dir.L then Dir.R else Dir.L


trait SnailfishNumber:
  def replace(path: List[Dir], elem: SnailfishNumber): SnailfishNumber =
    modify(path)(_ => elem)

  def modify(path: List[Dir])(f: SnailfishNumber => SnailfishNumber): SnailfishNumber =
    (this, path) match
      case (Pair(l, r), Dir.L :: Nil) => Pair(f(l), r)
      case (Pair(l, r), Dir.R :: Nil) => Pair(l, f(r))
      case (Pair(l, r), Dir.L :: rest) => Pair(l.modify(rest)(f), r)
      case (Pair(l, r), Dir.R :: rest) => Pair(l, r.modify(rest)(f))
      case (snailnum, _) => f(snailnum) // either Lit or end of path

  def magnitude: Int =
    this match
      case Pair(l, r) => l.magnitude * 3 + r.magnitude * 2
      case Lit(x) => x

  infix def +(right: SnailfishNumber): SnailfishNumber =
    Snailfish.reduce(Pair(this, right))

case class Lit(value: Int) extends SnailfishNumber:
  override def toString = s"$value"
case class Pair(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber:
  override def toString = s"[$left,$right]"

object Snailfish:

  def snailfishNumber: Parser[SnailfishNumber] =
    (
      int.map(Lit.apply) ||
        squareBrackets(
          (snailfishNumber <~ char(',')) ~ snailfishNumber
        ).map(Pair.apply.tupled)
    ).map(_.merge)


  def reduce(sn: SnailfishNumber): SnailfishNumber =
    tryExplode(sn).orElse(trySplit(sn)).map(reduce).getOrElse(sn)

  def trySplit(sn: SnailfishNumber): Option[SnailfishNumber] =
    sn match
      case Pair(l, r) =>
        trySplit(l).map(newL => Pair(newL, r))
          .orElse(trySplit(r).map(newR => Pair(l, newR)))
      case Lit(x) if x < 10 => None
      case Lit(x) =>  Some(Pair(Lit(x / 2), Lit(x / 2 + x % 2)))


  def tryExplode(sn: SnailfishNumber): Option[SnailfishNumber] =
    def findPath(path: List[Dir], depth: Int, elem: SnailfishNumber): Option[(List[Dir], Int, Int)] = elem match
        case Pair(l, r) if depth < 4 =>
          val left = findPath(Dir.L :: path, depth + 1, l)
          if left.nonEmpty then left
          else findPath(Dir.R :: path, depth + 1, r)
        case Pair(Lit(l),Lit(r)) => (path.reverse, l, r).some
        case Lit(_) => None

    def nextNeighbour(dir: Dir, path: List[Dir]): Option[List[Dir]] =
      val rpath = path.reverse
      val pos = rpath.indexOf(dir.opposite)
      if pos < 0 then None
      else
        val (pre, _ :: post) = rpath.splitAt(pos)
        Some((pre.map(_ => dir.opposite) ++ (dir:: post)).reverse)

    def add(value: Int, dir: Dir)(snail: SnailfishNumber) : SnailfishNumber = snail match
      case Lit(x) => Lit(x + value)
      case Pair(l, r) =>
        if dir == Dir.R then Pair(l, add(value, dir)(r))
        else Pair(add(value, dir)(l), r)

    findPath(List(), 0, sn).map { (path, l, r) =>
      val zeroed = sn.replace(path, Lit(0))
      val left = nextNeighbour(Dir.L, path)
      val right = nextNeighbour(Dir.R, path)
      val leftAdded = left.map(p => zeroed.modify(p)(add(l, Dir.R))).getOrElse(zeroed)
      right.map(p => leftAdded.modify(p)(add(r, Dir.L))).getOrElse(leftAdded)
    }
