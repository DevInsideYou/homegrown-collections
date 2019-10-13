package homegrown.collections

trait FoldableFactory[
    +Element,
    SubtypeOfFoldableFactory[+Element] <: FoldableFactory[
      Element,
      SubtypeOfFoldableFactory
    ]]
    extends Foldable[Element] {
  protected def factory: Factory[SubtypeOfFoldableFactory]

  def add[Super >: Element](input: Super): SubtypeOfFoldableFactory[Super]

  final def filterNot(
      predicate: Element => Boolean
    ): SubtypeOfFoldableFactory[Element] =
    filter(!predicate(_))

  def filter(predicate: Element => Boolean): SubtypeOfFoldableFactory[Element] =
    foldRight[SubtypeOfFoldableFactory[Element]](factory.empty) {
      (current, acc) =>
        if (predicate(current))
          acc.add(current)
        else
          acc
    }

  def takeWhile(
      predicate: Element => Boolean
    ): SubtypeOfFoldableFactory[Element] =
    foldRight[SubtypeOfFoldableFactory[Element]](factory.empty) {
      (current, acc) =>
        if (predicate(current))
          acc.add(current)
        else
          factory.empty
    }

  final def withFilter(
      predicate: Element => Boolean
    ): FoldableFactory.Wrapper[Element, SubtypeOfFoldableFactory] =
    new FoldableFactory.Wrapper(this, predicate)

  def map[Result](
      function: Element => Result
    ): SubtypeOfFoldableFactory[Result] =
    foldRight[SubtypeOfFoldableFactory[Result]](factory.empty) {
      (current, acc) =>
        acc.add(function(current))
    }

  def flatMap[Result](
      function: Element => Foldable[Result]
    ): SubtypeOfFoldableFactory[Result] =
    foldRight[SubtypeOfFoldableFactory[Result]](factory.empty) {
      (current, acc) =>
        function(current).foldRight(acc) { (current, acc) =>
          acc.add(current)
        }
    }

  def flatten[Result](
      implicit
      view: Element => Foldable[Result]
    ): SubtypeOfFoldableFactory[Result] =
    foldRight[SubtypeOfFoldableFactory[Result]](factory.empty) {
      (current, acc) =>
        view(current).foldRight(acc) { (current, acc) =>
          acc.add(current)
        }
    }
}

object FoldableFactory {
  final class Wrapper[
      +Element,
      SubtypeOfFoldableFactory[+Element] <: FoldableFactory[
        Element,
        SubtypeOfFoldableFactory
      ]
    ](
      foldableFactory: FoldableFactory[Element, SubtypeOfFoldableFactory],
      predicate: Element => Boolean) {
    final def foreach[Result](function: Element => Result): Unit = {
      foldableFactory.foldLeft(()) { (_, current) =>
        if (predicate(current))
          function(current)
      }
    }

    final def map[Result](
        function: Element => Result
      ): SubtypeOfFoldableFactory[Result] =
      foldableFactory.foldRight[SubtypeOfFoldableFactory[Result]](
        foldableFactory.factory.empty
      ) { (current, acc) =>
        if (predicate(current))
          acc.add(function(current))
        else
          acc
      }

    final def flatMap[Result](
        function: Element => Foldable[Result]
      ): SubtypeOfFoldableFactory[Result] =
      foldableFactory.foldRight[SubtypeOfFoldableFactory[Result]](
        foldableFactory.factory.empty
      ) { (current, acc) =>
        if (predicate(current))
          function(current).foldRight(acc) { (current, acc) =>
            acc.add(current)
          } else
          acc
      }
  }
}
