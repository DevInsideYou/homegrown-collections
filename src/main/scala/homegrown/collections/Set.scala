package homegrown.collections

import Trampoline._

sealed abstract class Set[+Element] extends FoldableFactory[Element, Set] {
  import Set._

  final override protected def factory: Factory[Set] =
    Set

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final /*override*/ def containsOriginal[Super >: Element](input: Super): Boolean =
    this match {
      case Empty =>
        false // JVM pop

      case NonEmpty(left, element, right) =>
        if (input == element)
          true // JVM pop
        else if (input.hashCode <= element.hashCode)
          left.contains(input) // JVM (push & pop)
        else
          right.contains(input) // JVM (push & pop)
    }

  final /*override*/ def containsLoop[Super >: Element](input: Super): Boolean = {
    @scala.annotation.tailrec
    def loop(set: Set[Element] /*, acc: Boolean*/ ): Boolean =
      set match {
        case Set.Empty =>
          false // JVM pop

        case Set.NonEmpty(left, element, right) =>
          if (input == element)
            true // JVM pop
          else if (input.hashCode <= element.hashCode)
            loop(left) // JVM (push & pop)
          else
            loop(right) // JVM (push & pop)
      }

    loop(this)
  }

  final override def contains[Super >: Element](input: Super): Boolean = {
    @scala.annotation.tailrec
    def loop(stack: Stack[Set[Element]]): Boolean = stack match {
      case Stack.Empty =>
        false // JVM pop

      case Stack.NonEmpty(set, otherSetsOnTheStack) => set match {
        case Set.Empty() =>
          loop(otherSetsOnTheStack) // pop + JVM (push & pop)

        case Set.NonEmpty(left, element, right) =>
          if (input == element)
            true // JVM pop
          else if (input.hashCode <= element.hashCode)
            loop(otherSetsOnTheStack.push(left)) // push + JVM (push & pop)
          else
            loop(otherSetsOnTheStack.push(right)) // push + JVM (push & pop)
      }
    }

    loop(Stack.empty.push(this))
  }

  final /*override*/ def foldOriginal[Result](seed: Result)(function: (Result, Element) => Result): Result =
    this match {
      case Empty() =>
        seed // JVM pop

      case NonEmpty(left, element, right) =>
        val currentResult = function(seed, element)

        val rightResult = right.fold(currentResult)(function) // JVM (push & pop)
        left.fold(rightResult)(function) // JVM (push & pop)
    }

  final override def fold[Result](seed: Result)(function: (Result, Element) => Result): Result = {
    @scala.annotation.tailrec
    def loop(stack: Stack[Set[Element]], acc: Result): Result = stack match {
      case Stack.Empty =>
        acc // JVM pop

      case Stack.NonEmpty(set, otherSetsOnTheStack) => set match {
        case Set.Empty() =>
          loop(otherSetsOnTheStack, acc) // pop + JVM (push & pop)

        case Set.NonEmpty(left, element, right) =>
          loop(otherSetsOnTheStack.push(right).push(left), function(acc, element)) // push 2x + JVM (push & pop)
      }
    }

    loop(Stack.empty.push(this), seed)
  }

  final /*override*/ def addOriginal[Super >: Element](input: Super): Set[Super] =
    this match {
      case Empty =>
        NonEmpty(empty, input, empty)

      case nonEmpty @ NonEmpty(left, element, right) =>
        if (input == element)
          this
        else if (input.hashCode <= element.hashCode)
          nonEmpty.copy(left = left.add(input))
        else
          nonEmpty.copy(right = right.add(input))
    }

  final /*override*/ def addStack[Super >: Element](input: Super): Set[Super] = {
    def path(set: Set[Element]): Path[Element] = {
      @scala.annotation.tailrec
      def loop(s: Set[Element], path: Path[Element]): Path[Element] = s match {
        case Set.Empty() =>
          path // JVM pop

        case nonEmpty @ Set.NonEmpty(left, element, right) =>
          if (input == element)
            path.push(Center(nonEmpty)) // push + JVM (push & pop)
          else if (input.hashCode <= element.hashCode)
            loop(left, path.push(Left(nonEmpty))) // push + JVM (push & pop)
          else
            loop(right, path.push(Right(nonEmpty))) // push + JVM (push & pop)
      }

      loop(set, Stack.empty)
    }

    def rebuild(path: Path[Element]): Set[Super] = {
      @scala.annotation.tailrec
      def loop(p: Path[Element], acc: Set[Super]): Set[Super] = p match {
        case Stack.Empty =>
          acc // JVM pop

        case Stack.NonEmpty(direction, otherDirectionsOnTheStack) =>
          loop(
            p   = otherDirectionsOnTheStack, // pop
            acc = direction match {
              case Left(nonEmpty)   => nonEmpty.copy(left = acc) // JVM (push & pop)
              case Center(nonEmpty) => nonEmpty // JVM pop
              case Right(nonEmpty)  => nonEmpty.copy(right = acc) // JVM (push & pop)
            }
          )
      }

      loop(path, NonEmpty(empty, input, empty))
    }

    rebuild(path(this))
  }

  // final override def add[Super >: Element](input: Super): Set[Super] = {
  //   @scala.annotation.tailrec
  //   def loop(s: Set[Element], continuation: Set[Super] => Trampoline[Set[Super]]): Trampoline[Set[Super]] = s match {
  //     case Set.Empty() =>
  //       continuation(NonEmpty(empty, input, empty))

  //     case nonEmpty @ Set.NonEmpty(left, element, right) =>
  //       if (input == element)
  //         continuation(nonEmpty)
  //       else if (input.hashCode <= element.hashCode)
  //         loop(left, acc => tailcall(continuation(nonEmpty.copy(left = acc))))
  //       else
  //         loop(right, acc => tailcall(continuation(nonEmpty.copy(right = acc))))
  //   }

  //   loop(this, done).result
  // }

  final override def add[Super >: Element](input: Super): Set[Super] = {
    var set: Set[Element] = this
    var continuation: (Set[Super] => Trampoline[Set[Super]]) = done

    while (set.nonEmpty) {
      val (nonEmpty @ Set.NonEmpty(left, element, right)) = set

      if (input == element)
        return this // same as continuation(nonEmpty)
      else {
        val closedContinuation = continuation

        if (input.hashCode <= element.hashCode) {
          set = left
          continuation = acc => tailcall(closedContinuation(nonEmpty.copy(left = acc)))
        }
        else {
          set = right
          continuation = acc => tailcall(closedContinuation(nonEmpty.copy(right = acc)))
        }
      }
    }

    continuation(NonEmpty(empty, input, empty)).result
  }

  final override def remove[Super >: Element](input: Super): Set[Super] = {
    @scala.annotation.tailrec
    def loop(s: Set[Element], continuation: Set[Super] => Set[Super]): Set[Super] = s match {
      case Set.Empty() =>
        continuation(empty)

      case nonEmpty @ Set.NonEmpty(left, element, right) =>
        if (input == element)
          continuation(left.union(right))
        else if (input.hashCode <= element.hashCode)
          loop(left, acc => continuation(nonEmpty.copy(left = acc)))
        else
          loop(right, acc => continuation(nonEmpty.copy(right = acc)))
    }

    loop(this, identity)
  }

  final /*override*/ def removeOriginal[Super >: Element](input: Super): Set[Super] =
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

  final /*override*/ def removeStack[Super >: Element](input: Super): Set[Super] = {
    def path(set: Set[Element]): Path[Element] = {
      @scala.annotation.tailrec
      def loop(s: Set[Element], path: Path[Element]): Path[Element] = s match {
        case Set.Empty() =>
          path // JVM pop

        case nonEmpty @ Set.NonEmpty(left, element, right) =>
          if (input == element)
            path.push(Center(nonEmpty)) // push + JVM (push & pop)
          else if (input.hashCode <= element.hashCode)
            loop(left, path.push(Left(nonEmpty))) // push + JVM (push & pop)
          else
            loop(right, path.push(Right(nonEmpty))) // push + JVM (push & pop)
      }

      loop(set, Stack.empty)
    }

    def rebuild(path: Path[Element]): Set[Super] = {
      @scala.annotation.tailrec
      def loop(p: Path[Element], acc: Set[Super]): Set[Super] = p match {
        case Stack.Empty =>
          acc // JVM pop

        case Stack.NonEmpty(direction, otherDirectionsOnTheStack) =>
          loop(
            p   = otherDirectionsOnTheStack, // pop
            acc = direction match {
              case Left(nonEmpty)                       => nonEmpty.copy(left = acc) // JVM (push & pop)
              case Center(Set.NonEmpty(left, _, right)) => left.union(right) // JVM pop
              case Right(nonEmpty)                      => nonEmpty.copy(right = acc) // JVM (push & pop)
            }
          )
      }

      loop(path, empty)
    }

    rebuild(path(this))
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

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty.type]

  final def nonEmpty: Boolean =
    !isEmpty

  def isSingleton: Boolean

  def sample: Option[Element]

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
  private final case class NonEmpty[+Element](left: Set[Element], element: Element, right: Set[Element]) extends Set[Element] {
    final def isSingleton: Boolean =
      left.isEmpty && right.isEmpty

    final override def sample: Option[Element] =
      Some(element)

    final override def toString: String =
      "{ " + element + splitByCommaSpace(left) + splitByCommaSpace(right) + " }"

    private[this] def splitByCommaSpace(input: Set[Element]) =
      input.fold("") { (acc, current) =>
        s"$acc, $current"
      }
  }

  private object Empty extends Set[Nothing] {
    /** case Empty => causes stack overflows in methods like fold
      * case Empty() => does not
      */
    def unapply[Element](set: Set[Element]): Boolean =
      set.isInstanceOf[Empty.type]

    final override def isSingleton: Boolean =
      false

    final override def sample: Option[Nothing] =
      None

    final override def toString: String =
      "{}"
  }

  final override def empty: Set[Nothing] = Empty

  private type Path[Element] = Stack[Direction[Element]]

  private sealed trait Direction[Element] extends Any
  private final case class Left[Element](nonEmpty: NonEmpty[Element]) extends AnyVal with Direction[Element]
  private final case class Center[Element](nonEmpty: NonEmpty[Element]) extends AnyVal with Direction[Element]
  private final case class Right[Element](nonEmpty: NonEmpty[Element]) extends AnyVal with Direction[Element]

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}
