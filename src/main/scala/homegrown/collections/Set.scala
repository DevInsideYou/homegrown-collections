package homegrown.collections

sealed trait Set[+Element] extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  @scala.annotation.tailrec
  final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    if (isEmpty)
      seed
    else
      otherElementsOrThrowException.fold(function(seed, elementOrThrowException))(function)

  final override def add[Super >: Element](input: Super): Set[Super] =
    fold(NonEmpty(input, empty)) { (acc, current) =>
      if (current == input)
        acc
      else
        NonEmpty(current, acc)
    }

  final override def remove[Super >: Element](input: Super): Set[Super] =
    fold[Set[Super]](empty) { (acc, current) =>
      if (current == input)
        acc
      else
        NonEmpty(current, acc)
    }

  final def union[Super >: Element](that: Set[Super]): Set[Super] =
    fold(that)(_ add _)

  final def intersection(predicate: Element => Boolean): Set[Element] =
    filter(predicate)

  final def difference(predicate: Element => Boolean): Set[Element] =
    fold[Set[Element]](empty) { (acc, current) =>
      if (predicate(current))
        acc
      else
        acc.add(current)
    }

  final def isSubsetOf(predicate: Element => Boolean): Boolean =
    forall(predicate)

  final def isSupersetOf[Super >: Element](that: Set[Super]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean = other match {
    case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
    case _                  => false
  }

  final override def hashCode: Int =
    fold(41)(_ + _.hashCode)

  final override def toString: String =
    if (isEmpty)
      "{}"
    else {
      val otherElementsSplitByCommaSpace =
        otherElementsOrThrowException.fold("") { (acc, current) =>
          s"$acc, $current"
        }

      "{ " + elementOrThrowException + otherElementsSplitByCommaSpace + " }"
    }

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty.type]

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean =
    nonEmpty && otherElementsOrThrowException.isEmpty

  def sample: Option[Element] =
    if (isEmpty)
      None
    else
      Some(elementOrThrowException)

  private[this] lazy val (elementOrThrowException, otherElementsOrThrowException) = {
    val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
    val element = nonEmptySet.element
    val otherElements = nonEmptySet.otherElements

    element -> otherElements
  }
}

object Set extends Factory[Set] {
  private final case class NonEmpty[Element](element: Element, otherElements: Set[Element]) extends Set[Element]

  private object NonEmpty {
    private[this] def unapply(any: Any): Option[(String, Any)] =
      patternMatchingNotSupported
  }

  private object Empty extends Set[Nothing] {
    private[this] def unapply(any: Any): Option[(String, Any)] =
      patternMatchingNotSupported
  }

  private[this] def unapply(any: Any): Option[(String, Any)] =
    patternMatchingNotSupported

  private[this] def patternMatchingNotSupported: Nothing =
    sys.error("pattern matching on Sets is expensive and therefore not supported")

  final override def empty: Set[Nothing] = Empty

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
