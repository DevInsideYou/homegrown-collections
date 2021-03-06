package homegrown
package collections

import mathlibrary._

trait Foldable[+Element] {
  def foldLeft[Result](
      seed: Result
    )(
      function: (Result, => Element) => Result
    ): Result

  def foldRight[Result](
      seed: => Result
    )(
      function: (=> Element, => Result) => Result
    ): Result

  @inline def aggregated[Super >: Element: Monoid]: Super =
    foldLeft(seed = Monoid[Super].uniqueIdentityElement) { (acc, current) =>
      Monoid[Super].operation(acc, current)
    }

  def size: Int =
    foldLeft(0) { (acc, _) =>
      acc + 1
    }

  final def doesNotContain[Super >: Element](input: Super): Boolean =
    !contains(input)

  def contains[Super >: Element](input: Super): Boolean =
    exists(_ == input)

  final def doesNotExist(predicate: Element => Boolean): Boolean =
    !exists(predicate)

  def exists(predicate: Element => Boolean): Boolean =
    foldRight(false)(predicate(_) || _)

  final def notForall(predicate: Element => Boolean): Boolean =
    !forall(predicate)

  def forall(predicate: Element => Boolean): Boolean =
    foldRight(true)(predicate(_) && _)

  def foreach[Result](function: Element => Result): Unit = {
    foldLeft(()) { (_, current) =>
      function(current)
    }
  }

  final def groupBy[Key](key: Element => Key): Map[Key, Set[Element]] =
    foldLeft[Map[Key, Set[Element]]](Map.empty) { (acc, current) =>
      val k: Key =
        key(current)

      val value: Set[Element] =
        acc(k)
          .map(_.add(current))
          .getOrElse(Set(current))

      acc.add(k -> value)
    }

  final def splitByCommaSpace: String =
    foldLeft("") { (acc, current) =>
      s"$acc, $current"
    }

  def find(predicate: Element => Boolean): Option[Element] =
    foldRight[Option[Element]](None) { (current, acc) =>
      if (predicate(current))
        Some(current)
      else
        acc
    }
}

object Foldable {
  implicit def viewFromIterableToFoldableFromHGC[Element](
      from: Iterable[Element]
    ): Foldable[Element] =
    new Foldable[Element] {
      final override def foldLeft[Result](
          seed: Result
        )(
          function: (Result, => Element) => Result
        ): Result =
        from.foldLeft(seed) { (acc, current) =>
          function(acc, current)
        }

      final override def foldRight[Result](
          seed: => Result
        )(
          function: (=> Element, => Result) => Result
        ): Result =
        from.foldRight(seed) { (current, acc) =>
          function(current, acc)
        }
    }

  // implicit def viewFromFoldableToIterableFromHGC[Element](from: Foldable[Element]): Iterable[Element] =
  //   new Iterable[Element] {
  //     final override def iterator: Iterator[Element] =
  //       new Iterator[Element] {
  //         private[this] var timeline: Timeline[Element] =
  //           from.foldRight[Timeline[Element]](Timeline.End)(_ #:: _)

  //         final override def hasNext: Boolean =
  //           timeline != Timeline.End

  //         final override def next(): Element = {
  //           val result = timeline.head.get

  //           timeline = timeline.tail

  //           result
  //         }
  //       }
  //   }
}
