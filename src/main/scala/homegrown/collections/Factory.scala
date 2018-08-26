package homegrown.collections

trait Factory[SubtypeOfFoldableFactory[+E] <: FoldableFactory[E, SubtypeOfFoldableFactory]] {
  final def apply[Element](element: Element, otherElements: Element*): SubtypeOfFoldableFactory[Element] =
    otherElements.foldLeft[SubtypeOfFoldableFactory[Element]](empty.add(element))(_ add _)

  def empty: SubtypeOfFoldableFactory[Nothing]
}
