package homegrown.collections

trait Foldable[Element] {
  def fold[Result](seed: Result)(function: (Result, Element) => Result): Result

  def size: Int =
    fold(0) { (acc, _) =>
      acc + 1
    }

  final def doesNotContain(input: Element): Boolean =
    !contains(input)

  def contains(input: Element): Boolean =
    exists(_ == input)

  final def doesNotExist(predicate: Element => Boolean): Boolean =
    !exists(predicate)

  def exists(predicate: Element => Boolean): Boolean =
    fold(false)(_ || predicate(_))

  final def notForall(predicate: Element => Boolean): Boolean =
    !forall(predicate)

  def forall(predicate: Element => Boolean): Boolean =
    fold(true)(_ && predicate(_))

  def foreach[Result](function: Element => Result): Unit = {
    fold(()) { (_, current) =>
      function(current)
    }
  }

  final def groupBy[Key](key: Element => Key): Map[Key, Set[Element]] =
    fold[Map[Key, Set[Element]]](Map.empty) { (acc, current) =>
      val k: Key =
        key(current)

      val value: Set[Element] =
        acc(k)
          .map(_.add(current))
          .getOrElse(Set(current))

      acc.add(k -> value)
    }
}

object Foldable {
  implicit def viewFromTraversableToFoldableFromHGC[Element](from: Traversable[Element]): Foldable[Element] =
    new Foldable[Element] {
      final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
        from.foldLeft(seed)(function)
    }

  implicit def viewFromFoldableToTraversableFromHGC[Element](from: Foldable[Element]): Traversable[Element] =
    new Traversable[Element] {
      final override def foreach[Result](function: Element => Result): Unit = {
        from.foreach(function)
      }
    }
}
