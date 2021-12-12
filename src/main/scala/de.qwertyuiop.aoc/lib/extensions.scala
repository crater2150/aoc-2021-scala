package de.qwertyuiop.aoc.lib

/* for splitting input with separator lines */
extension [A](input: List[A])(using CanEqual[A,A])
  def split(separator: A, keepSeparator: Boolean = false): LazyList[List[A]] =
    input.span(_ != separator) match
        case (Nil, Nil)              => LazyList()
        case (h,   Nil)              => LazyList(h)
        case (h,   tail @ (_ :: t)) =>
          h #:: (if keepSeparator then tail else t).split(separator)

extension [A](iterable: Iterable[A])
  def occurrences: Map[A, Int] = iterable.groupMapReduce(identity)(_ => 1)(_ + _)

/* Using -Yexplicit-nulls isn't really ready for use with the java standard
 * library. e.g. String doesn't have `@NotNull` annotations for its methods
 */
extension (s: String)
  def splitOnce(regex: String): Option[(String, String)] =
    s.split(regex, 2) match
      case Array(a, b) => Some((a.nn, b.nn))
      case _ => None

  def splitNN(regex: String): List[String] =
    s.split(regex).nn.map(_.nn).toList

extension [K,V,W](map: Map[K,V])
  def mapValuesS(f: V => W): Map[K, W] = map.view.mapValues(f).toMap

extension [A](input: List[A])
  def countAll: Map[A, Int] = input.groupBy(identity).view.mapValues(_.size).toMap
