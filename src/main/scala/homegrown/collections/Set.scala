package homegrown.collections

final class Set[Element] private (
    val tree: Tree[Element]
) extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  final def apply(input: Element): Boolean =
    contains(input)

  final override def contains(input: Element): Boolean =
    tree.contains(input)

  final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result =
    tree.fold(seed)(function)

  final override def add(input: Element): Set[Element] =
    if (contains(input))
      this
    else
      Set(tree add input)

  final def remove(input: Element): Set[Element] =
    Set(tree remove input)

  final def union(that: Set[Element]): Set[Element] =
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

  final def isSupersetOf(that: Set[Element]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean =
    other match {
      case that: Set[Element] => this.isSubsetOf(that) && that.isSubsetOf(this)
      case _                  => false
    }

  final override def hashCode: Int =
    fold(41)(_ + _.hashCode)

  final override def toString: String = tree match {
    case Tree.Empty() =>
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
    case Tree.NonEmpty(Tree.Empty(), _, Tree.Empty()) => true
    case _ => false
  }

  final def sample: Option[Element] = tree match {
    case Tree.Empty()                 => None
    case Tree.NonEmpty(_, element, _) => Some(element)
  }
}

object Set extends Factory[Set] {
  final override def empty[Element]: Set[Element] =
    apply(Tree.empty[Element])

  private def apply[Element](tree: Tree[Element]): Set[Element] =
    new Set(tree)

  final def withCustomOrdering[Element](element: Element, otherElements: Element*)(ordering: Ordering[Element]): Set[Element] =
    otherElements.foldLeft[Set[Element]](withCustomOrdering(ordering).add(element))(_ add _)

  final def withCustomOrdering[Element](ordering: Ordering[Element]): Set[Element] =
    apply(Tree.withCustomOrdering(ordering))

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
