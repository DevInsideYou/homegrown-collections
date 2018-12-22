package homegrown.collections

sealed abstract class Set[+Element] extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final override def contains[Super >: Element](input: Super): Boolean =
    this match {
      case Empty =>
        false

      case NonEmpty(left, element, right) =>
        if (input == element)
          true
        else if (input.hashCode <= element.hashCode)
          left.contains(input)
        else
          right.contains(input)
    }

  final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    this match {
      case Empty() =>
        seed

      case NonEmpty(left, element, right) =>
        val currentResult = function(seed, element)
        val rightResult = right.fold(currentResult)(function)
        left.fold(rightResult)(function)
    }

  final override def add[Super >: Element](input: Super): Set[Super] =
    this match {
      case Empty =>
        NonEmpty(empty, input, empty)

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input == element)
          nonEmpty
        else if (input.hashCode <= element.hashCode)
          nonEmpty.copy(left = left.add(input))
        else
          nonEmpty.copy(right = right.add(input))
    }

  final def remove[Super >: Element](input: Super): Set[Super] =
    this match {
      case Empty =>
        empty

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input == element)
          left.union(right)
        else if (input.hashCode <= element.hashCode)
          nonEmpty.copy(left = left.remove(input))
        else
          nonEmpty.copy(right = right.remove(input))
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

  final override def equals(other: Any): Boolean =
    other match {
      case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
      case _                  => false
    }

  final override def hashCode: Int =
    fold(41)(_ + _.hashCode)

  final override def toString: String = this match {
    case Empty() =>
      "{}"

    case NonEmpty(left, element, right) =>
      "{ " + element + splitByCommaSpace(left) + splitByCommaSpace(right) + " }"
  }

  private[this] def splitByCommaSpace(input: Set[Element]) =
    input.fold("") { (acc, current) =>
      s"$acc, $current"
    }

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty.type]

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean = this match {
    case Empty() =>
      false

    case NonEmpty(left, _, right) =>
      left.isEmpty && right.isEmpty
  }

  final def sample: Option[Element] = this match {
    case Empty() =>
      None

    case NonEmpty(_, element, _) =>
      Some(element)
  }

  final def rendered: String = {
    def leftOrRight(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst)
        ""
      else if (isLeft)
        "└── "
      else
        "├── "

    def leftOrRightParent(isLeft: Boolean, isFirst: Boolean): String =
      if (isFirst)
        ""
      else if (isLeft)
        "    "
      else
        "│   "

    def loop(prefix: String, isLeft: Boolean, isFirst: Boolean, set: Set[Element]): String = {
      set match {
        case Empty() =>
          ""

        case NonEmpty(left, element, right) =>
          prefix + leftOrRight(isLeft, isFirst) + element + "\n" +
            loop(prefix + leftOrRightParent(isLeft, isFirst), isLeft  = false, isFirst = false, right) +
            loop(prefix + leftOrRightParent(isLeft, isFirst), isLeft  = true, isFirst = false, left)
      }
    }

    loop(
      prefix  = "",
      isLeft  = true,
      isFirst = true,
      set     = this
    )
  }
}

object Set extends Factory[Set] {
  private final case class NonEmpty[+Element](left: Set[Element], element: Element, right: Set[Element]) extends Set[Element]

  private object Empty extends Set[Nothing] {
    /** case Empty => causes stack overflows in methods like fold
      * case Empty() => does not
      */
    def unapply[Element](set: Set[Element]): Boolean =
      set.isInstanceOf[Empty.type]
  }

  final override def nothing: Set[Nothing] = Empty

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
