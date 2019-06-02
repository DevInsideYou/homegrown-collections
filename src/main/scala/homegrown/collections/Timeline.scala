package homegrown
package collections

import mathlibrary._

sealed abstract class Timeline[+Event]
  extends (Int => Option[Event]) {
  import Timeline._

  final override def apply(index: Int): Option[Event] = {
    @scala.annotation.tailrec
    def loop(
        timeline: Timeline[Event],
        count: Int
    ): Option[Event] =
      if (index < 0)
        None
      else if (count == index)
        timeline.head
      else
        loop(timeline.tail, count + 1)

    loop(this, 0)
  }

  final def head: Option[Event] =
    this match {
      case End =>
        None

      case NonEmpty(recentEvent, _) =>
        Some(recentEvent.unsafeRun())
    }

  final def tail: Timeline[Event] =
    this match {
      case End =>
        End

      case NonEmpty(_, previousEvents) =>
        previousEvents.unsafeRun()
    }

  final def isEmpty: Boolean =
    this.isInstanceOf[End.type]

  final def nonEmpty: Boolean =
    !isEmpty

  @scala.annotation.tailrec
  final def foldLeft[Result](seed: Result)(function: (Result, => Event) => Result): Result = this match {
    case End =>
      seed

    case NonEmpty(recentEvent, previousEvents) =>
      val currentResult = function(seed, recentEvent.unsafeRun())
      previousEvents.unsafeRun().foldLeft(currentResult)(function)
  }

  final def foldRight[Result](seed: => Result)(function: (=> Event, => Result) => Result): Result =
    this match {
      case End =>
        seed

      case NonEmpty(recentEvent, previousEvents) =>
        lazy val otherResult = previousEvents.unsafeRun().foldRight(seed)(function)
        function(recentEvent.unsafeRun(), otherResult)
    }

  def size: Int =
    foldLeft(0) { (acc, _) =>
      acc + 1
    }

  final def foreach[Result](function: Event => Result): Unit = {
    foldLeft(()) { (_, current) =>
      function(current)
    }
  }

  final def map[Result](function: Event => Result): Timeline[Result] =
    foldRight[Timeline[Result]](End)(function(_) #:: _)

  final def flatMap[Result](function: (=> Event) => Timeline[Result]): Timeline[Result] =
    foldRight[Timeline[Result]](End) { (current, acc) =>
      function(current).foldRight(acc)(_ #:: _)
    }

  final def flatten[Result](implicit view: (=> Event) => Timeline[Result]): Timeline[Result] =
    foldRight[Timeline[Result]](End) { (current, acc) =>
      view(current).foldRight(acc)(_ #:: _)
    }

  final def take(amount: Int): Timeline[Event] = {
    @scala.annotation.tailrec
    def loop(
        timeline: Timeline[Event],
        acc: Timeline[Event],
        count: Int
    ): Timeline[Event] = timeline match {
      case End =>
        acc

      case NonEmpty(recentEvent, previousEvents) =>
        if (count >= amount)
          acc
        else
          loop(
            timeline = previousEvents.unsafeRun(),
            acc      = recentEvent.unsafeRun() #:: acc,
            count    = count + 1
          )
    }

    loop(this, End, 0).reversed
  }

  final def reversed: Timeline[Event] =
    foldLeft[Timeline[Event]](End)(_ add _)

  final override def equals(other: Any): Boolean =
    other match {
      case that: Timeline[Event] =>
        if (this.isEmpty && that.isEmpty)
          true
        else if (this.isEmpty || that.isEmpty)
          false
        else {
          val NonEmpty(n1Head, n1Tail) = this
          val NonEmpty(n2Head, n2Tail) = that

          n1Head.unsafeRun() == n2Head.unsafeRun() && // format: OFF
          n1Tail.unsafeRun() == n2Tail.unsafeRun() // format: ON
        }

      case _ =>
        false
    }

  final override def toString: String =
    s"Timeline($toStringContent)"

  private[this] def toStringContent: String =
    this match {
      case End =>
        ""

      case NonEmpty(recentEvent, previousEvents) =>
        s"${recentEvent.unsafeRun()}, ${Console.GREEN}...${Console.RESET}"
    }

  final def zip[ThatEvent](that: Timeline[ThatEvent]): Timeline[(Event, ThatEvent)] =
    this match {
      case End =>
        End

      case NonEmpty(recentEvent, previousEvents) =>
        that match {
          case End =>
            End

          case NonEmpty(thatRecentEvent, thatPreviousEvents) =>
            previousEvents
              .unsafeRun()
              .zip(thatPreviousEvents.unsafeRun())
              .after(recentEvent.unsafeRun() -> thatRecentEvent.unsafeRun())
        }
    }

  final def interleave[Super >: Event](that: Timeline[Super]): Timeline[Super] =
    this match {
      case End =>
        that

      case NonEmpty(recentEvent, previousEvents) =>
        recentEvent.unsafeRun() #:: that.interleave(previousEvents.unsafeRun())
    }

  final def forced: List[Event] =
    foldRight[List[Event]](List.empty)(_ :: _)

  @inline final def drained: List[Event] =
    forced

  @inline final def toList: List[Event] =
    forced
}

object Timeline {
  /** Required for flatten*/
  implicit def force[A](a: => A): A = a

  final case class NonEmpty[+Event] private (
      recentEvent: IO[Event],
      previousEvents: IO[Timeline[Event]]
  ) extends Timeline[Event]

  object NonEmpty {
    def apply[Event](
        recentEvent: IO[Event],
        previousEvents: IO[Timeline[Event]]
    ): Timeline[Event] =
      new NonEmpty(
        recentEvent    = recentEvent.memoized,
        previousEvents = previousEvents.memoized
      )
  }

  case object End extends Timeline[Nothing]

  implicit final class TimelineOpsFromHGC[Event](timeline: => Timeline[Event]) {
    final def add[Super >: Event](input: => Super): Timeline[Super] =
      NonEmpty(
        recentEvent    = IO.pure(input),
        previousEvents = IO.pure(timeline)
      )

    @inline final def prepend[Super >: Event](input: => Super): Timeline[Super] =
      add(input)

    @inline final def after[Super >: Event](input: => Super): Timeline[Super] =
      add(input)

    @inline final def #::[Super >: Event](input: => Super): Timeline[Super] =
      add(input)

    final def addMany[Super >: Event](that: Timeline[Super]): Timeline[Super] =
      that.foldRight[Timeline[Super]](timeline)(_ #:: _)

    @inline final def prependMany[Super >: Event](that: Timeline[Super]): Timeline[Super] =
      addMany(that)

    @inline final def afterMany[Super >: Event](that: Timeline[Super]): Timeline[Super] =
      addMany(that)

    @inline final def #:::[Super >: Event](that: Timeline[Super]): Timeline[Super] =
      addMany(that)
  }

  final def apply[Event](recentEvent: => Event, followingEvents: IO[Event]*): Timeline[Event] =
    recentEvent #:: followingEvents.foldRight[Timeline[Event]](End)(_.unsafeRun() #:: _)

  final def apply[Event](
      first: => Event
  ): Timeline[Event] =
    first #:: Timeline.End

  final def apply[Event](
      first: => Event,
      second: => Event
  ): Timeline[Event] =
    first #:: apply(second)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event
  ): Timeline[Event] =
    first #:: apply(second, third)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event
  ): Timeline[Event] =
    first #:: apply(second, third, fourth)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event
  ): Timeline[Event] =
    first #:: apply(second, third, fourth, fifth)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event
  ): Timeline[Event] =
    first #:: apply(second, third, fourth, fifth, sixth)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event,
      seventh: => Event
  ): Timeline[Event] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event,
      seventh: => Event,
      eighth: => Event
  ): Timeline[Event] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event,
      seventh: => Event,
      eighth: => Event,
      ninth: => Event
  ): Timeline[Event] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth, ninth)

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event,
      seventh: => Event,
      eighth: => Event,
      ninth: => Event,
      tenth: => Event,
      followingEvents: IO[Event]*
  ): Timeline[Event] =
    first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth) #::: followingEvents.foldRight[Timeline[Event]](Timeline.End)(_.unsafeRun() #:: _)

  implicit def arbitrary[T](implicit arbitrary: Arbitrary[IO[T]]): Arbitrary[Timeline[T]] =
    Arbitrary(gen[T])

  def gen[T](implicit arbitrary: Arbitrary[IO[T]]): Gen[Timeline[T]] =
    Gen.containerOf[scala.LazyList, IO[T]](arbitrary.arbitrary)
      .map { lazyList =>
        if (lazyList.isEmpty)
          Timeline.End
        else
          Timeline(lazyList.head.unsafeRun(), lazyList.tail: _*)
      }

  def genNonEmpty[T](implicit arbitrary: Arbitrary[IO[T]]): Gen[Timeline[T]] =
    Gen.nonEmptyContainerOf[scala.LazyList, IO[T]](arbitrary.arbitrary)
      .map { lazyList =>
        if (lazyList.isEmpty)
          sys.error("should not happen")
        else
          Timeline(lazyList.head.unsafeRun(), lazyList.tail: _*)
      }
}
