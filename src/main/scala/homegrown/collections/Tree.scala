package homegrown.collections

sealed abstract class Tree[Element] extends FoldableFactory[Element, Tree] {
  import Tree._

  final override protected def factory: Factory[Tree] =
    Tree

  def ordering: Ordering[Element]
  private[this] lazy val ord = ordering
  import ord._

  final override def contains(input: Element): Boolean =
    this match {
      case Empty() =>
        false

      case NonEmpty(left, element, right) =>
        if (input == element)
          true
        else if (input <= element)
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

  final override def add(input: Element): Tree[Element] =
    this match {
      case empty @ Empty() =>
        NonEmpty(empty, input, empty)(ordering)

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input <= element)
          nonEmpty.copy(left = left.add(input))(ordering)
        else
          nonEmpty.copy(right = right.add(input))(ordering)
    }

  final def remove(input: Element): Tree[Element] =
    this match {
      case Empty() =>
        empty

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input == element)
          left.remove(input).union(right)
        else if (input <= element)
          nonEmpty.copy(left = left.remove(input))(ordering)
        else
          nonEmpty.copy(right = right.remove(input))(ordering)
    }

  final def union(that: Tree[Element]): Tree[Element] =
    fold(that)(_ add _)

  final override def hashCode: Int =
    fold(41)(_ + _.hashCode)

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty[Element]]

  final def nonEmpty: Boolean =
    !isEmpty

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

    def loop(prefix: String, isLeft: Boolean, isFirst: Boolean, set: Tree[Element]): String = {
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

object Tree extends Factory[Tree] {
  final case class NonEmpty[Element](left: Tree[Element], element: Element, right: Tree[Element])(override val ordering: Ordering[Element]) extends Tree[Element] {
    final override def productPrefix: String = "Tree.NonEmpty"
  }

  final case class Empty[Element]()(override val ordering: Ordering[Element]) extends Tree[Element] {
    final override def toString: String = "Tree.Empty"
  }

  final def withCustomOrdering[Element](element: Element, otherElements: Element*)(ordering: Ordering[Element]): Tree[Element] =
    otherElements.foldLeft[Tree[Element]](withCustomOrdering(ordering).add(element))(_ add _)

  final def withCustomOrdering[Element](ordering: Ordering[Element]): Tree[Element] =
    Empty()(ordering)

  final override def empty[Element]: Tree[Element] =
    Empty()(Ordering.by(_.hashCode))
}

object Main extends App {
  println("─" * 50)

  val firstTree: Set[Int] = Set.withCustomOrdering(1, 2, 3)(Ordering[Int])
  val secondTree: Set[Int] = Set.withCustomOrdering(1, 2, 3)(Ordering[Int].reverse)

  println(firstTree.union(secondTree).tree.rendered)
  println(firstTree.union(secondTree))
  println(secondTree.union(firstTree).tree.rendered)
  println(secondTree.union(firstTree))

  println("─" * 50)
}
