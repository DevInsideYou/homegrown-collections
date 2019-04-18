package homegrown.collections

sealed abstract class Tree[+Element] extends FoldableFactory[Element, Tree] {
  import Tree._

  final override protected def factory: Factory[Tree] =
    Tree

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

  final override def foldLeft[Result](seed: Result)(function: (Result, Element) => Result): Result =
    this match {
      case Empty =>
        seed

      case NonEmpty(left, element, right) =>
        val currentResult = function(seed, element)
        val rightResult = right.foldLeft(currentResult)(function)
        left.foldLeft(rightResult)(function)
    }

  final override def foldRight[Result](seed: => Result)(function: (Element, => Result) => Result): Result = this match {
    case Empty =>
      seed

    case NonEmpty(left, element, right) =>
      lazy val leftResult = left.foldRight(seed)(function)
      lazy val rightResult = right.foldRight(leftResult)(function)
      function(element, rightResult)
  }

  final override def add[Super >: Element](input: Super): Tree[Super] =
    this match {
      case Empty =>
        NonEmpty(empty, input, empty)

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input.hashCode <= element.hashCode)
          nonEmpty.copy(left = left.add(input))
        else
          nonEmpty.copy(right = right.add(input))
    }

  final def remove[Super >: Element](input: Super): Tree[Super] =
    this match {
      case Empty =>
        empty

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input == element)
          left.remove(input).union(right)
        else if (input.hashCode <= element.hashCode)
          nonEmpty.copy(left = left.remove(input))
        else
          nonEmpty.copy(right = right.remove(input))
    }

  final def union[Super >: Element](that: Tree[Super]): Tree[Super] =
    foldLeft(that)(_ add _)

  final override def hashCode: Int =
    foldLeft(41)(_ + _.hashCode)

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty.type]

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
        case Empty =>
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
  final case class NonEmpty[+Element](left: Tree[Element], element: Element, right: Tree[Element]) extends Tree[Element] {
    final override def productPrefix: String = "Tree.NonEmpty"
  }

  object Empty extends Tree[Nothing] {
    final override def toString: String = "Tree.Empty"
  }

  final override def nothing: Tree[Nothing] = Empty
}
