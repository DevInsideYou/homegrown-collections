package homegrown.collections

trait FoldableFactory[+Element, SubtypeOfFoldableFactory[+Element] <: FoldableFactory[Element, SubtypeOfFoldableFactory]]
  extends Foldable[Element] {
  protected def factory: Factory[SubtypeOfFoldableFactory]

  def add[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]

  final def filterNot(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    filter(!predicate(_))

  def filter(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    fold[SubtypeOfFoldableFactory[Element]](factory.empty) { (acc, current) =>
      if (predicate(current))
        acc.add(current)
      else
        acc
    }

  final def withFilter(predicate: Element => Boolean): FoldableFactory.Wrapper[Element, SubtypeOfFoldableFactory] =
    new FoldableFactory.Wrapper(this, predicate)

  def map[Result](function: Element => Result): SubtypeOfFoldableFactory[Result] =
    fold[SubtypeOfFoldableFactory[Result]](factory.empty)(_ add function(_))

  def flatMap[Result, F[_]](function: Element => F[Result])(implicit view: F[Result] => Foldable[Result]): SubtypeOfFoldableFactory[Result] =
    fold[SubtypeOfFoldableFactory[Result]](factory.empty) { (acc, current) =>
      view(function(current)).fold(acc)(_ add _)
    }

  def flatten[Result](implicit view: Element => Foldable[Result]): SubtypeOfFoldableFactory[Result] =
    fold[SubtypeOfFoldableFactory[Result]](factory.empty) { (acc, current) =>
      view(current).fold(acc)(_ add _)
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

    final def flatMap[Result, F[_]](function: Element => F[Result])(implicit view: F[Result] => Foldable[Result]): SubtypeOfFoldableFactory[Result] =
      foldableFactory.fold[SubtypeOfFoldableFactory[Result]](foldableFactory.factory.empty) { (acc, current) =>
        if (predicate(current))
          view(function(current)).fold(acc)(_ add _)
        else
          acc
      }
  }
}
