package homegrown.collections

final class Set[+Element] private (val tree: Tree[Element]) extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final override def contains[Super >: Element](input: Super): Boolean =
    tree.contains(input)

  final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    tree.fold(seed)(function)

  final override def add[Super >: Element](input: Super): Set[Super] =
    if (contains(input))
      this
    else
      Set(tree add input)

  final def remove[Super >: Element](input: Super): Set[Super] =
    Set(tree remove input)

  final def union[Super >: Element](that: Set[Super]): Set[Super] =
    Set(this.tree union that.tree)

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

  final override def toString: String = tree match {
    case Tree.Empty =>
      "{}"

    case Tree.NonEmpty(left, element, right) =>
      "{ " + element + splitByCommaSpace(left) + splitByCommaSpace(right) + " }"
  }

  private[this] def splitByCommaSpace(input: Tree[Element]) =
    input.fold("") { (acc, current) =>
      s"$acc, $current"
    }

  final def isEmpty: Boolean =
    tree.isEmpty

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean = tree match {
    case Tree.NonEmpty(Tree.Empty, _, Tree.Empty) => true
    case _                                        => false
  }

  final def sample: Option[Element] = tree match {
    case Tree.Empty                   => None
    case Tree.NonEmpty(_, element, _) => Some(element)
  }
}

object Set extends Factory[Set] {
  final override def nothing: Set[Nothing] =
    apply(Tree.empty)

  private def apply[Element](tree: Tree[Element]): Set[Element] =
    new Set(tree)

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
