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
