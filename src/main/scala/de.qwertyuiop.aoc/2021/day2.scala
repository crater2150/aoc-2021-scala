package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

import Vectors.*

def day2(using InputSource): Unit =
  val commands = input()
  println(simpleCommands(commands))
  println(aimCommands(commands))


def simpleCommands(commands: List[String]) =
  val finalPos = commands.map{
    case s"forward $x" => Vec2D(x.toInt, 0)
    case s"down $y" => Vec2D(0, y.toInt)
    case s"up $y" => Vec2D(0, - y.toInt)
  }.combineAll
  s"(1) Final position: ${finalPos}\nResult: ${finalPos.x * finalPos.y}"

def aimCommands(commands: List[String]) =
  val finalPos = commands.map{
    case s"forward $x" => Vec3D(x.toInt, x.toInt, 0)
    case s"down $aim" => Vec3D(0, 0, aim.toInt)
    case s"up $aim" => Vec3D(0, 0, - aim.toInt)
  }.foldLeft(Vec3D(0,0,0))((pos, command) => Vec3D(pos.x + command.x, pos.y + pos.z * command.y, pos.z + command.z))
  s"(2) Final position: ${finalPos}\nResult: ${finalPos.x * finalPos.y}"
