package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given

def day10(using InputSource): Unit =
  val source = input()
  val opposites = Map(
    ')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<',
    '(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>'
  )
  val errorPoints = Map(
    ')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137,
  )
  val correctionPoints = Map(
    ')' -> 1L, ']' -> 2L, '}' -> 3L, '>' -> 4L,
  )

  def validate(line: String) =
    def step(open: List[Char], remain: List[Char]): Either[Char, List[Char]] =
      (open, remain) match 
        case (o, (next @ ('('|'['|'{'|'<')) :: rest)  => step(next :: o, rest)
        case (lastOpen :: tailOpen, (next @ (')'|']'|'}'|'>')) :: rest)  =>
          if opposites(next) == lastOpen then
            step(tailOpen, rest)
          else
            Left(next)
        case (o, Nil) => Right(o)
        case (o, illegalChar:: r) => Left(illegalChar)

    step(List(), line.toList)

  val checked = source.map(validate)
  val totalErrorPoints = checked.collect{ case Left(c) => errorPoints(c)}.sum
  val completionsForValid = checked.collect{ case Right(open) => open.map(opposites andThen correctionPoints)}
  val allCompletionPoints = completionsForValid.map(_.foldLeft(0L)((total, next) => total * 5L + next)).sorted
  println("All Points:")
  allCompletionPoints.foreach(println)
  println("---")
  val middleCompletionPoints = allCompletionPoints(allCompletionPoints.size / 2)

  println(totalErrorPoints)
  println(middleCompletionPoints)

