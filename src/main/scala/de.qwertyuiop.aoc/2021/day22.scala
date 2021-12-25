package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import CuboidReactor.*
import collection.immutable.NumericRange

def day22(using InputSource): Unit =
  val steps = input{ case s"$on x=$xs..$xe,y=$ys..$ye,z=$zs..$ze" =>
    ReactorCommand(on == "on", Cuboid(xs.toLong to xe.toLong, ys.toLong to ye.toLong, zs.toLong to ze.toLong)) }

  val onAfterInit = steps.foldLeft(CuboidReactor(Vector()))(_ initExec _).activeCount
  val onAfterReboot = steps.foldLeft(CuboidReactor(Vector()))(_ exec _).activeCount
  println(s"On after Init: $onAfterInit\nOn after Reboot: $onAfterReboot:")

type LRange = NumericRange[Long]
val EmptyLRange = 0L to -1L
extension (r: LRange)
  def &(o: LRange) = (r.start max o.start) to (r.end min o.end)
  def -(o: LRange): Vector[LRange] =
    val intersect = r & o
    if intersect.isEmpty then r.pure
    else if intersect == r then Vector.empty
    else
      (if intersect.start > r.start then Vector(r.start to (intersect.start - 1L)) else Vector())
      ++
      (if intersect.end < r.end then Vector((intersect.end + 1L) to r.end) else Vector())

case class Cuboid(xRange: LRange, yRange: LRange, zRange: LRange):
  val volume: Long = (xRange.length.toLong) * (yRange.length.toLong) * (zRange.length.toLong)

  def &(other: Cuboid) = Cuboid(xRange & other.xRange, yRange & other.yRange, zRange & other.zRange)
  def &&(other: Cuboid): Option[Cuboid] = Some(this & other).filterNot(_.isEmpty)
  def -(other: Cuboid): Vector[Cuboid] = (this && other).fold(Vector(this))(
    intersect =>
      (xRange - intersect.xRange).map(x => this.copy(xRange = x)) ++
      (yRange - intersect.yRange).map(y => this.copy(xRange = intersect.xRange, yRange = y)) ++
      (zRange - intersect.zRange).map(z => this.copy(xRange = intersect.xRange, yRange = intersect.yRange, zRange = z))
  )

  def isEmpty = xRange.isEmpty || yRange.isEmpty || zRange.isEmpty

object Cuboid:
  def empty = Cuboid(EmptyLRange, EmptyLRange, EmptyLRange)

case class ReactorCommand(on: Boolean, cuboid: Cuboid)

case class CuboidReactor(active: Vector[Cuboid]):
  def +(cuboid: Cuboid) = CuboidReactor(active.flatMap(_ - cuboid) :+ cuboid)
  def -(cuboid: Cuboid) = CuboidReactor(active.flatMap(_ - cuboid))
  def exec(cmd: ReactorCommand) = if cmd.on then this + cmd.cuboid else this - cmd.cuboid
  def initExec(cmd: ReactorCommand) = (cmd.cuboid && initLimit).map(c => exec(cmd.copy(cuboid = c))).getOrElse(this)

  def activeCount: Long = active.map(_.volume).sum

object CuboidReactor:
  val initRange = -50L to 50L
  val initLimit = Cuboid(initRange, initRange, initRange)
