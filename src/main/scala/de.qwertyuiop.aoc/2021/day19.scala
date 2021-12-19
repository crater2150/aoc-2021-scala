package de.qwertyuiop.aoc.`2021`

import de.qwertyuiop.aoc.lib.*
import cats.*, cats.implicits.given
import Vectors.{*, given}
import BeaconScanners.*

def day19(using InputSource): Unit =
  val scanners = input().split("").collect{
    case s"--- scanner $index ---" :: coords => index.toInt -> coords.collect((Vec3D.parse[Int] _ ).unlift)
  }.toMap

  val normalizedScanners =
    for (id, coords) <- scanners
    yield
      val dists = coords.zipWithIndex.combinations(2)
        .collect{ case List((a, ai),(b, bi)) => normalize(a - b) -> Set(ai, bi) }
      id -> dists.toMap

  val overlapping = normalizedScanners.toVector.combinations(2).flatMap {
    case Vector(id1 -> dists1, id2 -> dists2) =>
      val overlapDists =  (dists1.keySet & dists2.keySet)
      val overlaps = overlapDists.flatMap(dists1).size
      val beaconMapping = overlapDists.toVector.flatMap { dist =>
          val List(a1, b1) = dists1(dist).toList
          val List(a2, b2) = dists2(dist).toList
          Vector(a1 -> a2, a1 -> b2, b1 -> a2, b1 -> b2)
      }.occurrences.filter(_._2 > 1).map(_._1).toMap
      Option.when(overlaps >= 12)((id1, id2, beaconMapping))
    case _ => sys.error("combinations(2) is broken")
  }.toVector

  val translations = overlapping.map { (a, b, beacons) =>
    val overlapOrig = beacons.map((k,v) => scanners(a)(k) -> scanners(b)(v))
    val overlapIndexed = overlapOrig.toVector
    val Vector(a0 -> b0, a1 -> b1) = overlapOrig.take(2).toVector
    val aDist = a0 - a1
    val bDist = b0 - b1
    val sign = aDist.combine(bDist)(_ / _)
    val (origin, neighbour) = overlapOrig.head
    Map(a -> List(b -> (origin - (neighbour ⊙ sign))))
  }.combineAll
  translations.foreach(println)
  val cleaned = translations.mapValuesS(_.filter(translations contains _._1))

  //cleaned.foreach((k, v) => println(s"from $k: ${v.size} translations: ${v.map(_._1)}"))

object BeaconScanners:
  def normalize(v: Vec3D[Int]): Vec3D[Int] =
    val Seq(x, y, z) = Seq(v.x, v.y, v.z).map(_.abs).sorted
    Vec3D(x, y, z)
