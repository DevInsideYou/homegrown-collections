package homegrown.collections

sealed trait Set[+Element] /*extends (Element => Boolean)*/ {
  import Set._

  final /*override*/ def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final def doesNotContain[Super >: Element](input: Super): Boolean =
    !contains(input)

  final def contains[Super >: Element](input: Super): Boolean =
    exists(_ == input)

  final def doesNotExist(predicate: Element => Boolean): Boolean =
    !exists(predicate)

  final def exists(predicate: Element => Boolean): Boolean =
    fold(false)(_ || predicate(_))

  final def notForall(predicate: Element => Boolean): Boolean =
    !forall(predicate)

  final def forall(predicate: Element => Boolean): Boolean =
    fold(true)(_ && predicate(_))

  final def add[Super >: Element](input: Super): Set[Super] =
    fold(NonEmpty(input, empty)) { (acc, current) =>
      if (current == input)
        acc
      else
        NonEmpty(current, acc)
    }

  final def remove[Super >: Element](input: Super): Set[Super] =
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

  final def filter(predicate: Element => Boolean): Set[Element] =
    fold[Set[Element]](empty) { (acc, current) =>
      if (predicate(current))
        acc.add(current)
      else
        acc
    }

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

  final def size: Int =
    fold(0) { (acc, _) =>
      acc + 1
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

  final def foreach[Result](function: Element => Result): Unit = {
    fold(()) { (_, current) =>
      function(current)
    }
  }

  final def map[Result](function: Element => Result): Set[Result] =
    fold[Set[Result]](empty)(_ add function(_))

  final def flatMap[Result](function: Element => Set[Result]): Set[Result] =
    fold[Set[Result]](empty) { (acc, current) =>
      function(current).fold(acc)(_ add _)
    }

  @scala.annotation.tailrec
  final def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    if (isEmpty)
      seed
    else
      otherElementsOrThrowException.fold(function(seed, elementOrThrowException))(function)

  private[this] lazy val (elementOrThrowException, otherElementsOrThrowException) = {
    val nonEmptySet = this.asInstanceOf[NonEmpty[Element]]
    val element = nonEmptySet.element
    val otherElements = nonEmptySet.otherElements

    element -> otherElements
  }
}

object Set {
  def apply[Element](element: Element, otherElements: Element*): Set[Element] =
    otherElements.foldLeft[Set[Element]](empty.add(element))(_ add _)

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

  def empty: Set[Nothing] = Empty

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
