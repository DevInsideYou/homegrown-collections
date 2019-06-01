package homegrown.collections

sealed abstract class Timeline[+Event] {
  import Timeline._

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

  final def foldRight[Result](seed: => Result)(function: (=> Event, => Result) => Result): Result =
    this match {
      case End =>
        seed

      case NonEmpty(event, previousEvents) =>
        lazy val otherResult = previousEvents.unsafeRun().foldRight(seed)(function)
        function(event.unsafeRun(), otherResult)
    }

  final def foreach[Result](function: Event => Result): Unit = {
    this match {
      case End =>

      case NonEmpty(recentEvent, previousEvents) =>
        function(recentEvent.unsafeRun())
        previousEvents.unsafeRun().foreach(function)
    }
  }

  final def map[Result](function: Event => Result): Timeline[Result] =
    foldRight[Timeline[Result]](End)(function(_) #:: _)

  final def flatMap[Result](function: (=> Event) => Timeline[Result]): Timeline[Result] =
    foldRight[Timeline[Result]](End) { (current, acc) =>
      function(current).foldRight(acc)(_ #:: _)
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

  final def reversed: Timeline[Event] = {
    @scala.annotation.tailrec
    def loop(
        timeline: Timeline[Event],
        acc: Timeline[Event]
    ): Timeline[Event] = timeline match {
      case End =>
        acc

      case NonEmpty(recentEvent, previousEvents) =>
        loop(
          timeline = previousEvents.unsafeRun(),
          acc      = recentEvent.unsafeRun() #:: acc
        )
    }

    loop(this, End)
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
            lazy val head =
              recentEvent.unsafeRun() -> thatRecentEvent.unsafeRun()

            lazy val tail =
              previousEvents.unsafeRun() zip thatPreviousEvents.unsafeRun()

            head #:: tail
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
  final case class NonEmpty[+Event](
      recentEvent: IO[Event],
      previousEvents: IO[Timeline[Event]]
  ) extends Timeline[Event]

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
}
