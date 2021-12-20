package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import Vectors.*
import CanYouEnhanceThat.*

import java.nio.file.{Files, Paths}

type Image = Map[Vec2D[Int], Char]
def day20(using InputSource): Unit =
  val algo :: _ :: values = input(_.map(c => (if c == '#' then '1' else '0')).toVector)

  val image = (for (line, x) <- values.zipWithIndex ; (chr, y)  <- line.zipWithIndex yield Vec2D(x, y) -> chr).toMap

  // ZOOM (out), ENHANCE!
  def enhanceN(steps: Int) = enhance(algo, image, 0, values.size - 1)(steps)(renderPBM(-steps, values.size - 1 + steps))

  val (enhanced, enhancedRenders)         = enhanceN(2)
  val (veryEnhanced, veryEnhancedRenders) = enhanceN(50)

  /* Visualization!
   * Uncomment, run this, then run:
   *
   *   % ffmpeg -r 5 -f image2 -s ${size}x${size} -i /tmp/%02d.pbm -vcodec libx264 -crf 25  -pix_fmt yuv420p ENHANCE.mp4
   *
   * where $size = iterations * 2 + values.size
   */
  //veryEnhancedRenders.reverse.zipWithIndex.foreach((pbm, i) => Files.writeString(Paths.get(f"/tmp/$i%02d.pbm"), pbm))
      
  println(s"After 2 iterations: ${enhanced.count((_, c) => c == '1')} lights on")
  println(s"After 50 iterations: ${veryEnhanced.count((_, c) => c == '1')} lights on")


object CanYouEnhanceThat:
  extension (c: Char)
    def flip = if c == '1' then '0' else '1'

  def enhance[T](algo: Vector[Char], image: Image, min: Int, max: Int, outer: Char = '0', renders: List[T] = Nil)(steps: Int)(renderer: Image => T): (Image, List[T]) =
    def lookup(pos: Vec2D[Int]) =
      val binStr = pos.kernel3x3.map(p => image.getOrElse(p, outer)).mkString
      algo(Integer.parseInt(binStr, 2))
    
    val rendersNew = renderer(image) :: renders
    if(steps == 0) (image, rendersNew)
    else
      val newImage = (
        for 
          x <- (min - 1) to (max + 1)
          y <- (min - 1) to (max + 1)
        yield
          val p = Vec2D(x,y)
          p -> lookup(p)
        ).toMap
      enhance(algo, newImage, min - 1, max + 1, if algo(0) == '1' then outer.flip else outer, rendersNew)(steps - 1)(renderer)


  /**
   * Output an Image to the terminal
   */
  def printImage(image: Image) =
    val min = image.keys.minBy(_.x).x
    val max = image.keys.maxBy(_.x).x
    print("\u001B[H")
    for x <- min - 2 to max + 2 do
      for y <- min - 2 to max + 2 do
        print(image.get(Vec2D(x,y)).map(c => if c == '1' then '#' else ' ').getOrElse(' '))
      println()

  /**
   * Render image as a PBM file. min and max set the range of coordinates to render.
   * This doesn't take the blinking outer part into account, to prevent seizures.
   */
  def renderPBM(min: Int, max: Int)(image: Image) =
    val width = max - min + 1
    val even = width % 2
    s"P1\n${width + even} ${width + even}\n" +
    (
      for x <- min to max + even yield
      (
        for y <- min to max + even yield image.getOrElse(Vec2D(x,y), '0')
      ).mkString(" ")
    ).mkString("\n")

  def noop(image: Image): Null = null
