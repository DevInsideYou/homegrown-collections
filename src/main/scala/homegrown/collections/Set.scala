package homegrown.collections

sealed abstract class Set[+Element] extends FoldableFactory[Element, Set] {
  import Set._
  import Set.Color._

  final override protected def factory: Factory[Set] =
    Set

  final def apply[Super >: Element](input: Super): Boolean =
    contains(input)

  final override def contains[Super >: Element](input: Super): Boolean =
    this match {
      case Empty =>
        false

      case NonEmpty(color, left, element, right) =>
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

      case NonEmpty(color, left, element, right) =>
        val currentResult = function(seed, element)
        val rightResult = right.fold(currentResult)(function)
        left.fold(rightResult)(function)
    }

  final override def add[Super >: Element](input: Super): Set[Super] = {
    val Okasaki = new Okasaki[Super]

    def ins(set: Set[Super]): Set[Super] = {
      set match {
        case Empty =>
          NonEmpty(Red, empty, input, empty)

        case NonEmpty(color, left, element, right) =>
          if (input == element)
            set
          else if (input.hashCode <= element.hashCode)
            Okasaki.balance(NonEmpty(color, ins(left), element, right))
          else
            Okasaki.balance(NonEmpty(color, left, element, ins(right)))
      }
    }

    Okasaki.makeBlack(ins(this))
  }

  final override def remove[Super >: Element](input: Super): Set[Super] =
    this match {
      case Empty =>
        empty

      case NonEmpty(color, left, element, right) =>
        if (input == element)
          left.union(right)
        else if (input.hashCode <= element.hashCode)
          NonEmpty(color, left.remove(input), element, right)
        else
          NonEmpty(color, left, element, right.remove(input))
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

        case NonEmpty(color, left, element, right) =>
          prefix + leftOrRight(isLeft, isFirst) + color.paint(element) + "\n" +
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

  protected def color: Color
}

object Set extends Factory[Set] {
  private final case class NonEmpty[+Element](color: Color, left: Set[Element], element: Element, right: Set[Element]) extends Set[Element] {
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

    final override protected def color: Color =
      Color.Black
  }

  final override def empty: Set[Nothing] = Empty

  private[Set] sealed abstract class Color {
    def paint(element: Any): String
  }

  private[Set] object Color {
    case object Red extends Color {
      final override def paint(element: Any): String =
        Console.RED + element + Console.RESET
    }

    case object Black extends Color {
      final override def paint(element: Any): String =
        Console.BLUE + element + Console.RESET
    }
  }

  private[Set] class Okasaki[S] {
    import Color._

    private def solution[E](
        a: Set[E],
        b: Set[E],
        c: Set[E],
        d: Set[E],
        x: E,
        y: E,
        z: E
    ): Set[E] =
      NonEmpty(
        color   = Red,
        left    = NonEmpty(color   = Black, left = a, element = x, right = b),
        element = y,
        right   = NonEmpty(color   = Black, left = c, element = z, right = d)
      )

    def makeBlack(set: Set[S]): Set[S] = set match {
      case Empty() => set

      case NonEmpty(_, left, element, right) =>
        NonEmpty(Black, left, element, right)
    }

    private type Case = PartialFunction[Set[S], Set[S]]

    private lazy val top: Case = {
      case NonEmpty(Black, NonEmpty(Red, a, x, NonEmpty(Red, b, y, c)), z, d) =>
        solution(a, b, c, d, x, y, z)
    }

    private lazy val right: Case = {
      case NonEmpty(Black, a, x, NonEmpty(Red, b, y, NonEmpty(Red, c, z, d))) =>
        solution(a, b, c, d, x, y, z)
    }

    private lazy val bottom: Case = {
      case NonEmpty(Black, a, x, NonEmpty(Red, NonEmpty(Red, b, y, c), z, d)) =>
        solution(a, b, c, d, x, y, z)
    }

    private lazy val left: Case = {
      case NonEmpty(Black, NonEmpty(Red, NonEmpty(Red, a, x, b), y, c), z, d) =>
        solution(a, b, c, d, x, y, z)
    }

    private lazy val center: Case = {
      case balanced => balanced
    }

    lazy val balance: Case =
      top orElse right orElse bottom orElse left orElse center
  }

  implicit def SetCanBeUsedAsFunction1[Element](set: Set[Element]): Element => Boolean =
    set.apply
}

object Main extends App {
  println("-" * 50)

  println(Set(0, 1 to 19: _*).rendered)

  println("-" * 50)
}
