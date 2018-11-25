package homegrown.collections

trait FoldableFactory[+Element, SubtypeOfFoldableFactory[+Element] <: FoldableFactory[Element, SubtypeOfFoldableFactory]]
  extends Foldable[Element] {
  protected def factory: Factory[SubtypeOfFoldableFactory]

  def add[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]

  // Technically not required but seems plausible to be left here for the symmetry with add.
  // So let's leave it here for now and see in the future if we end up needing it.
  def remove[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]

  final def filterNot(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    filter(!predicate(_))

  final def withFilter(predicate: Element => Boolean): FoldableFactory.Wrapper[Element, SubtypeOfFoldableFactory] =
    new FoldableFactory.Wrapper(this, predicate)

  def filter(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    fold[SubtypeOfFoldableFactory[Element]](factory.empty) { (acc, current) =>
      if (predicate(current))
        acc.add(current)
      else
        acc
    }

  def map[Result](function: Element => Result): SubtypeOfFoldableFactory[Result] =
    fold[SubtypeOfFoldableFactory[Result]](factory.empty)(_ add function(_))

  def flatMap[Result](function: Element => Foldable[Result]): SubtypeOfFoldableFactory[Result] =
    fold[SubtypeOfFoldableFactory[Result]](factory.empty) { (acc, current) =>
      function(current).fold(acc)(_ add _)
    }
}

object FoldableFactory {
  final class Wrapper[+Element, SubtypeOfFoldableFactory[+Element] <: FoldableFactory[Element, SubtypeOfFoldableFactory]](
      foldableFactory: FoldableFactory[Element, SubtypeOfFoldableFactory],
      predicate: Element => Boolean
  ) {
    final def foreach[Result](function: Element => Result): Unit = {
      foldableFactory.fold(()) { (_, current) =>
        if (predicate(current))
          function(current)
      }
    }

    final def map[Result](function: Element => Result): SubtypeOfFoldableFactory[Result] =
      foldableFactory.fold[SubtypeOfFoldableFactory[Result]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current))
          acc.add(function(current))
        else
          acc
      }

    final def flatMap[Result](function: Element => Foldable[Result]): SubtypeOfFoldableFactory[Result] =
      foldableFactory.fold[SubtypeOfFoldableFactory[Result]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current))
          function(current).fold(acc)(_ add _)
        else
          acc
      }
  }
}
