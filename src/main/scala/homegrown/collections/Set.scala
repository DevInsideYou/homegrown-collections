package homegrown
package collections

import mathlibrary._

final class Set[+Element] private (
    val tree: Tree[Element]
) extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final override def contains[Super >: Element](input: Super): Boolean =
    tree.contains(input)

  final override def foldLeft[Result](seed: Result)(function: (Result, Element) => Result): Result =
    tree.foldLeft(seed)(function)

  final override def foldRight[Result](seed: => Result)(function: (Element, => Result) => Result): Result =
    tree.foldRight(seed)(function)

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
    foldLeft[Set[Element]](empty) { (acc, current) =>
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
    foldLeft(41)(_ + _.hashCode)

  final override def toString: String =
    s"HGCSet($toStringContent)"

  private[this] def toStringContent: String = tree match {
    case Tree.Empty =>
      ""

    case Tree.NonEmpty(left, element, right) =>
      s"${element}${left.splitByCommaSpace}${right.splitByCommaSpace}"
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

  implicit def arbitrary[T: Arbitrary]: Arbitrary[Set[T]] =
    Arbitrary(gen[T])

  def gen[T: Arbitrary]: Gen[Set[T]] =
    Gen.listOf(Arbitrary.arbitrary[T]).map {
      case Nil          => Set.empty[T]
      case head :: tail => Set(head, tail: _*)
    }

  def genNonEmpty[T: Arbitrary]: Gen[Set[T]] =
    Gen.nonEmptyListOf(Arbitrary.arbitrary[T]).map {
      case Nil          => sys.error("should not happen")
      case head :: tail => Set(head, tail: _*)
    }

  implicit def Union[A: Arbitrary]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      final override protected lazy val arbitrary: Arbitrary[Set[A]] =
        implicitly[Arbitrary[Set[A]]]

      final override lazy val operation: ClosedBinaryOperation[Set[A]] =
        _ union _

      final override lazy val uniqueIdentityElement: Set[A] =
        empty[A]

    }
}
