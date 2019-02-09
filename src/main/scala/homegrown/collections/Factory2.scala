package homegrown.collections

trait Factory2[SubtypeOfFoldableFactory[K, Value] <: FoldableFactory2[K, Value, SubtypeOfFoldableFactory]] {
  final def apply[Key, Value](element: (Key, Value), otherElements: (Key, Value)*): SubtypeOfFoldableFactory[Key, Value] =
    otherElements.foldLeft[SubtypeOfFoldableFactory[Key, Value]](empty.add(element))(_ add _)

  def empty[Key, Value]: SubtypeOfFoldableFactory[Key, Value]
}
