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

  object Vec2D:
    def apply[T](x: T, y: T): Vec2D[T] = (x,y)
    def allCoords(matrix: IndexedSeq[IndexedSeq[_]]): Iterable[Vec2D[Int]] =
      (
        for
          x <- 0 until matrix.size
          y <- 0 until matrix.headOption.map(_.size).getOrElse(0)
        yield (x, y)
      )

    def parse[T: Numeric](in: String): Option[Vec2D[T]] =
      in match
        case s"$x,$y" =>
          for xt <- Numeric[T].parseString(x)
              yt <- Numeric[T].parseString(y)
          yield Vec2D(xt,yt)
        case _ => None

  object Vec3D:
    def apply[T](x: T, y: T, z: T): Vec3D[T] = (x,y,z)

    def parse[T: Numeric](in: String): Option[Vec3D[T]] =
      in match
        case s"$x,$y,$z" =>
          for xt <- Numeric[T].parseString(x)
              yt <- Numeric[T].parseString(y)
              zt <- Numeric[T].parseString(z)
          yield Vec3D(xt,yt,zt)
        case _ => None

  object Vec4D:
    def apply[T](x: T, y: T, z: T, w: T): Vec4D[T] = (x,y,z,w)

    def parse[T: Numeric](in: String): Option[Vec4D[T]] =
      in match
        case s"$x,$y,$z,$w" =>
          for xt <- Numeric[T].parseString(x)
              yt <- Numeric[T].parseString(y)
              zt <- Numeric[T].parseString(z)
              wt <- Numeric[T].parseString(w)
          yield Vec4D(xt,yt,zt,wt)
        case _ => None

  trait Vec[V, E]:
    def parse(in: String): Option[V]

    extension (a: V)
      def +(b: V): V
      def -(b: V): V
      def *(scale: E): V

      def combine(b: V)(f: (E, E) => E): V

      def neighbours: Vector[V]

  given [T : Show]: Show[Vec2D[T]] with
    def show(v: Vec2D[T]): String = show"Vec2D(${v._1}, ${v._2})"
  given [T : Show]: Show[Vec3D[T]] with
    def show(v: Vec3D[T]): String = show"Vec3D(${v._1}, ${v._2}, ${v._3})"
  given [T : Show]: Show[Vec4D[T]] with
    def show(v: Vec4D[T]): String = show"Vec4D(${v._1}, ${v._2}, ${v._3}, ${v._4})"

  given Functor[Vec2D] with
    def map[A, B](fa: Vec2D[A])(f: A => B): Vec2D[B] = Vec2D(f(fa._1), f(fa._2))

  given Functor[Vec3D] with
    def map[A, B](fa: Vec3D[A])(f: A => B): Vec3D[B] = Vec3D(f(fa._1), f(fa._2), f(fa._3))

  given Functor[Vec4D] with
    def map[A, B](fa: Vec4D[A])(f: A => B): Vec4D[B] = Vec4D(f(fa._1), f(fa._2), f(fa._3), f(fa._4))

  given [T: Numeric]: Vec[Vec2D[T], T] with
    def parse(in: String): Option[Vec2D[T]] = Vec2D.parse(in)

    extension (a: Vec2D[T])
      def +(b: Vec2D[T]): Vec2D[T] = (a.x + b.x, a.y + b.y)
      def -(b: Vec2D[T]): Vec2D[T] = (a.x - b.x, a.y - b.y)
      def *(s: T): Vec2D[T] = (a.x * s, a.y * s)

      def combine(b: Vec2D[T])(f: (T, T) => T): Vec2D[T] = Vec2D(f(a.x, b.x), f(a.y, b.y))

      def x: T = a._1
      def y: T = a._2

      def rot(deg: Dir): Vec2D[T] = deg match
        case North => (-a.y, a.x)
        case West => (-a.x, -a.y)
        case South => (a.y, -a.x)
        case East => a

      def left(deg: Int): Vec2D[T] = repeat[Vec2D[T]](deg / 90)(_.rotLeft)(a)
      def right(deg: Int): Vec2D[T] = repeat[Vec2D[T]](deg / 90)(_.rotRight)(a)

      def rotLeft: Vec2D[T] = (-a.y, a.x)
      def rotRight: Vec2D[T] = (a.y, -a.x)

      def move(dir: Dir, dist: T): Vec2D[T] = dir match
        case North => (a.x, a.y + dist)
        case South => (a.x, a.y - dist)
        case East => (a.x + dist, a.y)
        case West => (a.x - dist, a.y)

      def manhattan: T = a.x.abs + a._2.abs
      def neighbours: Vector[Vec2D[T]] =
        neighbourCoords(2).map(n => a + (n(0), n(1)))

      def kernel3x3: Vector[Vec2D[T]] =
        val (pre,post) = neighbours.splitAt(4)
        pre.appended(a) ++ post

      def orthoNeighbours(sizeX: T, sizeY: T)(using Ordering[T]): Vector[Vec2D[T]] =
        val n = summon[Numeric[T]]
        import math.Ordering.Implicits.infixOrderingOps
        val vb = collection.immutable.VectorBuilder[(T,T)]()
        if x > n.zero then vb += ((x - n.one, y))
        if x < sizeX - n.one then vb += ((x + n.one, y))
        if y > n.zero then vb += ((x, y - n.one))
        if y < sizeY - n.one then vb += ((x, y + n.one))
        vb.result()

      def orthoNeighbours: Vector[Vec2D[T]] =
        val n = summon[Numeric[T]]
        Vector((x - n.one, y), (x + n.one, y), (x, y - n.one), (x, y + n.one))

  given [T: Monoid]: Monoid[Vec2D[T]] = semiauto.monoid
  given [T: Monoid]: Monoid[Vec3D[T]] = semiauto.monoid
  given [T: Monoid]: Monoid[Vec4D[T]] = semiauto.monoid

  given [T: Numeric]: Vec[Vec3D[T], T] with
    def parse(in: String): Option[Vec3D[T]] = Vec3D.parse(in)

    extension (a: Vec3D[T])
      def +(b: Vec3D[T]): Vec3D[T] = (a.x + b.x, a.y + b.y, a.z + b.z)
      def -(b: Vec3D[T]): Vec3D[T] = (a.x - b.x, a.y - b.y, a.z - b.z)
      def *(s: T): Vec3D[T] = (a.x * s, a.y * s, a.z * s)

      def combine(b: Vec3D[T])(f: (T, T) => T): Vec3D[T] = Vec3D(f(a.x, b.x), f(a.y, b.y), f(a.z, b.z))

      /** elementwise multiplication */
      def ⊙(b: Vec3D[T]): Vec3D[T] = (a.x * b.x, a.y * b.y, a.z * b.z)

      def neighbours: Vector[Vec3D[T]] =
        neighbourCoords(3).map(n => a + (n(0), n(1), n(2)))

      def x: T = a._1
      def y: T = a._2
      def z: T = a._3

  given [T: Numeric]: Vec[Vec4D[T], T] with
    def parse(in: String): Option[Vec4D[T]] = Vec4D.parse(in)

    extension (a: Vec4D[T])
      def +(b: Vec4D[T]): Vec4D[T] = (a.x + b.x, a.y + b.y, a.z + b.z, a.w + b.w)
      def -(b: Vec4D[T]): Vec4D[T] = (a.x - b.x, a.y - b.y, a.z - b.z, a.w - b.w)
      def *(s: T): Vec4D[T] = (a.x * s, a.y * s, a.z * s, a.w * s)

      def combine(b: Vec4D[T])(f: (T, T) => T): Vec4D[T] = Vec4D(f(a.x, b.x), f(a.y, b.y), f(a.z, b.z), f(a.w, b.w))

      def neighbours: Vector[Vec4D[T]] =
        neighbourCoords(4).map(n => a + (n(0), n(1), n(2), n(3)))

      def x: T = a._1
      def y: T = a._2
      def z: T = a._3
      def w: T = a._4


  /* compute these only once per type and dimension*/
  import scala.collection.mutable
  private var _neighbourCache = mutable.Map[(Numeric[_], Int), Vector[Vector[_]]]()
  def neighbourCoords[T](dim: Int)(using n: Numeric[T]): Vector[Vector[T]] =
    _neighbourCache.get((n, dim)) match
      case None =>
        val self = Vector.fill(dim)(n.zero)
        val neighs = Vector.fill(dim)(Vector(-n.one, n.zero, n.one)).sequence[Vector, T]
          .filter(_ != self)
        _neighbourCache.put((n, dim), neighs)
        neighs
      case Some(neighs) => neighs.asInstanceOf[Vector[Vector[T]]]

  extension[A](v: IndexedSeq[IndexedSeq[A]]) def get(p: Vec2D[Int]): A = v(p.x)(p.y)
  extension[A](v: IndexedSeq[IndexedSeq[IndexedSeq[A]]]) def get(p: Vec3D[Int]): A = v(p.x)(p.y)(p.z)
  extension[A](v: IndexedSeq[IndexedSeq[IndexedSeq[IndexedSeq[A]]]]) def get(p: Vec4D[Int]): A = v(p.x)(p.y)(p.z)(p.w)

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
