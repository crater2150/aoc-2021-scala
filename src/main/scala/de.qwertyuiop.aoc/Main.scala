package de.qwertyuiop.aoc
import de.qwertyuiop.aoc.lib._
import de.qwertyuiop.aoc.`2021`._
@main def runDay(inputDir: String, day: Int, sample: Int*): Unit =
  given InputSource = inputSource(inputDir, day, sample.headOption)
  day match {
    case 1 => day1
    case 2 => day2
    case 3 => day3
    case 4 => day4
    case 5 => day5
    case 6 => day6
    case 7 => day7
    case 8 => day8
    case 9 => day9
    case 10 => day10
    case 11 => day11
    case 12 => day12
    case 13 => day13
    case 14 => day14
    case 15 => day15
    case 16 => day16
    case 17 => day17
    case 18 => day18
    case 19 => day19
    case 20 => day20
    case 21 => day21
    case 22 => day22
    case 23 => day23
    case 24 => day24
    case 25 => day25
    case _ => println("No such day implemented")
  }

def inputSource(inputDir:String, day: Int, sample: Option[Int]) =
  sample.map(InputSource.SampleLocation(inputDir, day, _))
    .getOrElse(InputSource.Location(inputDir, day))
