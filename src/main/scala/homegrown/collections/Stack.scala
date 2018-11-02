package homegrown.collections

sealed abstract class Stack[+Element] {
  import Stack._

  final def push[Super >: Element](input: Super): Stack[Super] =
    NonEmpty(input, this)

  final lazy val (peak, pop) = popElement

  final def popElement: (Option[Element], Stack[Element]) =
    this match {
      case Empty =>
        None -> empty

      case NonEmpty(element, otherElements) =>
        Some(element) -> otherElements
    }
}

object Stack {
  final case class NonEmpty[+Element](element: Element, otherElements: Stack[Element]) extends Stack[Element]
  final case object Empty extends Stack[Nothing]

  def empty: Stack[Nothing] = Empty
}
