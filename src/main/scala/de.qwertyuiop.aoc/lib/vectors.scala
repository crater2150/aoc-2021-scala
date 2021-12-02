package de.qwertyuiop.aoc.lib

import cats.*, cats.implicits.given
import cats.derived.semiauto

object Vectors: 
  import scala.Numeric.Implicits.given
  import scala.compiletime.ops.int.*
  import Directions.*

  opaque type Vec2D[T] = (T, T)
  opaque type Vec3D[T] = (T, T, T)
  opaque type Vec4D[T] = (T, T, T, T)

  def Vec2D[T](x: T, y: T): Vec2D[T] = (x,y)
  def Vec3D[T](x: T, y: T, z: T): Vec3D[T] = (x,y,z)
  def Vec4D[T](x: T, y: T, z: T, w: T): Vec4D[T] = (x,y,z,w)

  trait Vec[T]:
    extension (v: T)
      def +(w: T): T
      def neighbours: List[T]

  given [T : Show]: Show[Vec2D[T]] with
    def show(v: Vec2D[T]): String = show"Vec2D(${v._1}, ${v._2})"
  given [T : Show]: Show[Vec3D[T]] with
    def show(v: Vec3D[T]): String = show"Vec3D(${v._1}, ${v._2}, ${v._3})"
  given [T : Show]: Show[Vec4D[T]] with
    def show(v: Vec4D[T]): String = show"Vec4D(${v._1}, ${v._2}, ${v._3}, ${v._4})"

  given [T: Numeric]: Vec[Vec2D[T]] with
    extension (v: Vec2D[T])
      def +(w: Vec2D[T]): Vec2D[T] = (v._1 + w._1, v._2 + w._2)
      def -(w: Vec2D[T]): Vec2D[T] = (v._1 - w._1, v._2 - w._2)
      def *(x: T): Vec2D[T] = (v._1 * x, v._2 * x)

      def x: T = v._1
      def y: T = v._2

      def rot(deg: Dir): Vec2D[T] = deg match
        case North => (-v._2, v._1)
        case West => (-v._1, -v._2)
        case South => (v._2, -v._1)
        case East => v

      def left(deg: Int): Vec2D[T] = repeat[Vec2D[T]](deg / 90)(_.rotLeft)(v)
      def right(deg: Int): Vec2D[T] = repeat[Vec2D[T]](deg / 90)(_.rotRight)(v)

      def rotLeft: Vec2D[T] = (-v._2, v._1)
      def rotRight: Vec2D[T] = (v._2, -v._1)

      def move(dir: Dir, dist: T): Vec2D[T] = dir match
        case North => (v._1, v._2 + dist)
        case South => (v._1, v._2 - dist)
        case East => (v._1 + dist, v._2)
        case West => (v._1 - dist, v._2)

      def manhattan: T = v._1.abs + v._2.abs
      def neighbours: List[Vec2D[T]] =
        neighbourCoords(2).map(n => v + (n(0), n(1)))

  given [T: Monoid]: Monoid[Vec2D[T]] = semiauto.monoid
  given [T: Monoid]: Monoid[Vec3D[T]] = semiauto.monoid
  given [T: Monoid]: Monoid[Vec4D[T]] = semiauto.monoid


  given [T: Numeric]: Vec[Vec3D[T]] with
    extension (v: Vec3D[T])
      def +(w: Vec3D[T]): Vec3D[T] = (v._1 + w._1, v._2 + w._2, v._3 + w._3)
      def neighbours: List[Vec3D[T]] =
        neighbourCoords(3).map(n => v + (n(0), n(1), n(2)))

      def x: T = v._1
      def y: T = v._2
      def z: T = v._3

  given [T: Numeric]: Vec[Vec4D[T]] with
    extension (v: Vec4D[T])
      def +(u: Vec4D[T]): Vec4D[T] = (v._1 + u._1, v._2 + u._2, v._3 + u._3, v._4 + u._4)
      def neighbours: List[Vec4D[T]] =
        neighbourCoords(4).map(n => v + (n(0), n(1), n(2), n(3)))

      def x: T = v._1
      def y: T = v._2
      def z: T = v._3
      def w: T = v._4


  /* compute these only once per type and dimension*/
  import scala.collection.mutable
  private var _neighbourCache = mutable.Map[(Numeric[_], Int), List[List[_]]]()
  def neighbourCoords[T](dim: Int)(using n: Numeric[T]): List[List[T]] =
    _neighbourCache.get((n, dim)) match
      case None =>
        val self = List.fill(dim)(n.zero)
        val neighs = List.fill(dim)(List(-n.one, n.zero, n.one)).sequence[List, T]
          .filter(_ != self)
        _neighbourCache.put((n, dim), neighs)
        neighs
      case Some(neighs) => neighs.asInstanceOf[List[List[T]]]

object Directions:
  opaque type Dir = Int
  val East: Dir = 0
  val North: Dir = 90
  val West: Dir = 180
  val South: Dir = 270

  extension (c: Char)
    def cardinal: Dir = c match
      case 'E' => East
      case 'N' => North
      case 'W' => West
      case 'S' => South

  def Dir(deg: Int): Dir = (deg % 360 + 360) % 360

  private val rotationOrder = Vector(North, West, South, East, North, West)

  extension (dir: Dir)
    def +(deg: Int|Dir): Dir = (dir + deg) % 360
    def -(deg: Int|Dir): Dir = ((dir - deg) % 360 + 360) % 360
    def unary_- : Dir = 360 - dir
    def str: String = dir match
      case East  => "East"
      case North => "North"
      case West  => "West"
      case South => "South"

    def rotLeft: Dir = rotationOrder(rotationOrder.indexOf(dir) + 1)
    def rotRight: Dir = rotationOrder(rotationOrder.lastIndexOf(dir) - 1)
    def flip: Dir = rotationOrder(rotationOrder.indexOf(dir) + 2)

    def flipHor: Dir = dir match
      case West => East
      case East => West
      case o => o

    def flipVert: Dir = dir match
      case North => South
      case South => North
      case o => o

  given Show[Dir] with
    def show(d: Dir): String = d.str
