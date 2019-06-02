package homegrown
package collections

import mathlibrary._

sealed abstract class List[+Element]
  extends FoldableFactory[Element, List]
  with (Int => Option[Element]) {
  import List._

  final override protected def factory: Factory[List] =
    List

  final override def apply(index: Int): Option[Element] = {
    @scala.annotation.tailrec
    def loop(
        list: List[Element],
        count: Int
    ): Option[Element] =
      if (index < 0)
        None
      else if (count == index)
        list.head
      else
        loop(list.tail, count + 1)

    loop(this, 0)
  }

  final def isEmpty: Boolean =
    this.isInstanceOf[Empty.type]

  final def nonEmpty: Boolean =
    !isEmpty

  @scala.annotation.tailrec
  final override def foldLeft[Result](seed: Result)(function: (Result, => Element) => Result): Result = this match {
    case Empty =>
      seed

    case NonEmpty(element, otherElements) =>
      val currentResult = function(seed, element)
      otherElements.foldLeft(currentResult)(function)
  }

  final def reduceLeft[Result >: Element](function: (Result, => Element) => Result): Option[Result] =
    head.map { seed =>
      tail.foldLeft[Result](seed)(function)
    }

  final def reduceLeftOrThrowException[Result >: Element](function: (Result, => Element) => Result): Result =
    reduceLeft(function).get

  final def reduceRight[Result >: Element](function: (=> Element, => Result) => Result): Option[Result] =
    head.map { seed =>
      tail.foldRight[Result](seed)(function)
    }

  final def reduceRightOrThrowException[Result >: Element](function: (=> Element, => Result) => Result): Result =
    reduceRight(function).get

  final override def foldRight[Result](seed: => Result)(function: (=> Element, => Result) => Result): Result =
    this match {
      case Empty =>
        seed

      case NonEmpty(element, otherElements) =>
        lazy val otherResult = otherElements.foldRight(seed)(function)
        function(element, otherResult)
    }

  final def :::[Super >: Element](that: List[Super]): List[Super] =
    that.foldRight[List[Super]](this)(_ :: _)

  final def add[Super >: Element](input: Super): List[Super] =
    NonEmpty(input, this)

  @inline final def prepend[Super >: Element](input: Super): List[Super] =
    add(input)

  @inline final def ::[Super >: Element](input: Super): List[Super] =
    add(input)

  @inline final def push[Super >: Element](input: Super): List[Super] =
    add(input)

  final lazy val (head, tail) = popElement
  @inline final lazy val peek = head
  @inline final lazy val pop = tail

  final def popElement: (Option[Element], List[Element]) =
    this match {
      case Empty =>
        None -> empty

      case NonEmpty(element, otherElements) =>
        Some(element) -> otherElements
    }

  final def take(amount: Int): List[Element] = {
    @scala.annotation.tailrec
    def loop(
        list: List[Element],
        acc: List[Element],
        count: Int
    ): List[Element] = list match {
      case Empty =>
        acc

      case NonEmpty(element, otherElements) =>
        if (count >= amount)
          acc
        else
          loop(
            list  = otherElements,
            acc   = element :: acc,
            count = count + 1
          )
    }

    loop(this, empty, 0).reversed
  }

  final def reversed: List[Element] =
    foldLeft[List[Element]](empty)(_ add _)

  final override def toString: String =
    s"HGCList($toStringContent)"

  private[this] def toStringContent: String = this match {
    case Empty => ""
    case NonEmpty(element, otherElements) =>
      s"${element}${otherElements.splitByCommaSpace}"
  }

  final def zip[ThatElement](that: List[ThatElement]): List[(Element, ThatElement)] =
    this match {
      case Empty =>
        Empty

      case NonEmpty(element, otherElements) =>
        that match {
          case Empty =>
            Empty

          case NonEmpty(thatElement, thatOtherElements) =>
            element -> thatElement :: otherElements.zip(thatOtherElements)
        }
    }

  final def interleave[Super >: Element](that: List[Super]): List[Super] =
    this match {
      case Empty =>
        that

      case NonEmpty(element, otherElements) =>
        element :: that.interleave(otherElements)
    }
}

object List extends Factory[List] {
  final override def apply[Element](element: Element, otherElements: Element*): List[Element] =
    element :: otherElements.foldRight[List[Element]](empty)(_ :: _)

  def unapplySeq[Event](list: List[Event]): Option[scala.Seq[Event]] =
    if (list == null)
      None
    else
      Some(
        list.foldRight[scala.List[Event]](scala.List.empty)(_ :: _)
      )

  final case class NonEmpty[+Element](element: Element, otherElements: List[Element]) extends List[Element]
  final case object Empty extends List[Nothing]

  final override def nothing: List[Nothing] = Empty

  implicit def arbitrary[T: Arbitrary]: Arbitrary[List[T]] =
    Arbitrary(gen[T])

  def gen[T: Arbitrary]: Gen[List[T]] =
    Gen.listOf(Arbitrary.arbitrary[T]).map {
      case Nil          => List.empty[T]
      case head :: tail => List(head, tail: _*)
    }

  def genNonEmpty[T: Arbitrary]: Gen[List[T]] =
    Gen.nonEmptyListOf(Arbitrary.arbitrary[T]).map {
      case Nil          => sys.error("should not happen")
      case head :: tail => List(head, tail: _*)
    }

  implicit def Concatenation[A: Arbitrary]: Monoid[List[A]] =
    new Monoid[List[A]] {
      final override protected lazy val arbitrary: Arbitrary[List[A]] =
        implicitly[Arbitrary[List[A]]]

      final override lazy val operation: ClosedBinaryOperation[List[A]] =
        _ ::: _

      final override lazy val uniqueIdentityElement: List[A] =
        empty[A]
    }
}
