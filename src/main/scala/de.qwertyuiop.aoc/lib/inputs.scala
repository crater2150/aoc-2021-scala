package de.qwertyuiop.aoc.lib

import scala.io.Source

enum InputSource:
  case Location(inputDir: String, day: Int)
  case Fixed(lines: List[String])
  case SampleLocation(inputDir: String, day: Int, sample: Int)

def inputLines(using src: InputSource): Iterator[String] = 
  src match
    case InputSource.Location(inputDir, day) => scala.io.Source.fromFile(s"${inputDir}/day${day}.txt").getLines
    case InputSource.Fixed(input) => input.iterator
    case InputSource.SampleLocation(inputDir, day, sample) => scala.io.Source.fromFile(s"${inputDir}/day${day}-sample${sample}.txt").getLines


def simpleInput(using l: InputSource): String = inputLines.mkString

def input[A](format: String => A = identity)(using l: InputSource): List[A] =
  inputF(format)(List)

def inputF[A, C[_]](format: String => A = identity)(coll: collection.Factory[A, C[A]])(using l: InputSource): C[A] =
  inputLines.map(format).to(coll)

def flatInput[A](format: String => List[A] = List.apply)(using l: InputSource): List[A] =
  flatInputF(format)(List)

def flatInputF[A, C[_]](format: String => IterableOnce[A] = identity)(coll: collection.Factory[A, C[A]])(using l: InputSource): C[A] =
  inputLines.flatMap(format).to(coll)

def boolChar(trueChar: Char): String => Vector[Boolean] = _.map(_ == trueChar).toVector
