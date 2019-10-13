package homegrown
package collections

import mathlibrary._

import Timeline.Data

final class Timeline[+Event] private (
    private val data: IO[Data[Event]]
) extends Foldable[Event]
  with (Int => Option[Event]) {
  final override def apply(index: Int): Option[Event] =
    data.unsafeRun().apply(index)

  final def head: Option[Event] =
    data.unsafeRun().head

  final def tail: Timeline[Event] =
    Timeline(data.map(_.tail))

  final def isEmpty: Boolean =
    this == Timeline.End

  final def nonEmpty: Boolean =
    !isEmpty

  final def reduceLeft[Result >: Event](function: (Result, => Event) => Result): Option[Result] =
    head.map { seed =>
      tail.foldLeft[Result](seed)(function)
    }

  final def reduceLeftOrThrowException[Result >: Event](function: (Result, => Event) => Result): Result =
    reduceLeft(function).get

  final def reduceRight[Result >: Event](function: (=> Event, => Result) => Result): Option[Result] =
    head.map { seed =>
      tail.foldRight[Result](seed)(function)
    }

  final def reduceRightOrThrowException[Result >: Event](function: (=> Event, => Result) => Result): Result =
    reduceRight(function).get

  final override def foldLeft[Result](seed: Result)(function: (Result, => Event) => Result): Result =
    data.unsafeRun().foldLeft(seed)(function)

  final override def foldRight[Result](seed: => Result)(function: (=> Event, => Result) => Result): Result =
    data.unsafeRun().foldRight(seed)(function)

  final def take(amount: Int): Timeline[Event] =
    Timeline(data.map(_.take(amount)))

  final def reversed: Timeline[Event] =
    Timeline(data.map(_.reversed))

  final def filter(predicate: Event => Boolean): Timeline[Event] =
    Timeline(data.map(_.filter(predicate)))

  final def withFilter(predicate: Event => Boolean): Timeline[Event] =
    filter(predicate)

  final def takeWhile(predicate: Event => Boolean): Timeline[Event] =
    Timeline(data.map(_.takeWhile(predicate)))

  final def map[Result](function: Event => Result): Timeline[Result] =
    Timeline(data.map(_.map(function)))

  final def flatMap[Result](function: (=> Event) => Foldable[Result]): Timeline[Result] =
    Timeline(data.map(_.flatMap(function)))

  final def flatten[Result](implicit view: (=> Event) => Foldable[Result]): Timeline[Result] =
    Timeline(data.map(_.flatten))

  final override def equals(other: Any): Boolean =
    other match {
      case that: Timeline[Event] =>
        this.data.unsafeRun() == that.data.unsafeRun()

      case _ =>
        false
    }

  final override def toString: String =
    s"Timeline($toStringContent)"

  private[this] def toStringContent: String =
    this match {
      case Timeline.End =>
        ""

      case Timeline.NonEmpty(recentEvent, _) =>
        s"${recentEvent.unsafeRun()}, ${Console.GREEN}...${Console.RESET}"
    }

  final def zip[ThatEvent](that: Timeline[ThatEvent]): Timeline[(Event, ThatEvent)] =
    Timeline(data.map(_.zip(that.data.unsafeRun())))

  final def interleave[Super >: Event](that: Timeline[Super]): Timeline[Super] =
    Timeline(data.map(_.interleave(that.data.unsafeRun())))

  final def forced: List[Event] =
    data.unsafeRun().forced

  @inline final def drained: List[Event] =
    forced

  @inline final def toList: List[Event] =
    forced
}

object Timeline {
  /** Required for flatten*/
  implicit def force[A](a: => A): A = a

  final private def apply[Event](data: IO[Data[Event]]): Timeline[Event] =
    new Timeline(data.memoized)

  object NonEmpty {
    def apply[Event](
        recentEvent: IO[Event],
        followingEvents: IO[Timeline[Event]]
    ): Timeline[Event] =
      Timeline(
        followingEvents.map { timeline =>
          Data.NonEmpty(
            recentEvent     = recentEvent,
            followingEvents = timeline.data
          )
        }
      )

    def unapply[Event](timeline: Timeline[Event]): Option[(IO[Event], IO[Timeline[Event]])] =
      timeline.data.unsafeRun() match {
        case Data.End =>
          None

        case Data.NonEmpty(recentEvent, followingEvents) =>
          Some(recentEvent -> IO.pure(Timeline(followingEvents)))
      }
  }

  def unapplySeq[Event](timeline: Timeline[Event]): Option[scala.Seq[Event]] =
    if (timeline == null)
      None
    else
      Some(
        timeline.foldRight[scala.LazyList[Event]](scala.LazyList.empty)(_ #:: _)
      )

  final val End: Timeline[Nothing] =
    Timeline(IO.pure(Data.End))

  final def end[Event]: Timeline[Event] =
    End

  implicit final class TimelineOpsFromHGC[Event](timeline: => Timeline[Event]) {
    final def add[Super >: Event](input: => Super): Timeline[Super] =
      NonEmpty(
        recentEvent     = IO.pure(input),
        followingEvents = IO.pure(timeline)
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
    Timeline(IO.pure(Data(recentEvent, followingEvents: _*)))

  final def apply[Event](
      first: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first)))

  final def apply[Event](
      first: => Event,
      second: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first, second)))

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first, second, third)))

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first, second, third, fourth)))

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first, second, third, fourth, fifth)))

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth)))

  final def apply[Event](
      first: => Event,
      second: => Event,
      third: => Event,
      fourth: => Event,
      fifth: => Event,
      sixth: => Event,
      seventh: => Event
  ): Timeline[Event] =
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh)))

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
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh, eighth)))

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
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh, eighth, ninth)))

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
    Timeline(IO.pure(Data(first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth, followingEvents: _*)))

  case object Int {
    final def asc(from: Int): Timeline[Int] =
      from.timeline(_ + 1)

    final def desc(from: Int): Timeline[Int] =
      from.timeline(_ - 1)

    object Range {
      final def inclusive(from: Int, to: Int): Timeline[Int] =
        if (from > to)
          Timeline.End
        else
          from #:: inclusive(from + 1, to)

      final def exclusive(from: Int, to: Int): Timeline[Int] =
        if (from >= to)
          Timeline.End
        else
          from #:: exclusive(from + 1, to)
    }
  }

  @inline final def generateSame[Event](seed: => Event): Timeline[Event] =
    generate(seed)(identity)

  @inline final def generate[Event](seed: => Event)(
      next: Event => Event
  ): Timeline[Event] =
    canonicalGenerateTransform(seed)(next, identity)

  @inline final def generateTransform[Gen, Event](seed: => Gen)(
      next: (=> Gen) => (IO[Event], IO[Gen])
  ): Timeline[Event] =
    unfold(seed)(next andThen Some.apply)

  final def unfold[Gen, Event](seed: => Gen)(
      next: (=> Gen) => Option[(IO[Event], IO[Gen])]
  ): Timeline[Event] =
    Timeline(IO.pure(Data.unfold(seed)(next)))

  @inline final def canonicalGenerateTransform[Gen, Event](seed: => Gen)(
      next: Gen => Gen,
      transform: Gen => Event
  ): Timeline[Event] =
    canonicalUnfold(seed)(next, transform, _ => false)

  final def canonicalUnfold[Gen, Event](seed: => Gen)(
      next: Gen => Gen,
      transform: Gen => Event,
      shouldFinish: (=> Gen) => Boolean
  ): Timeline[Event] =
    Timeline(IO.pure(Data.canonicalUnfold(seed)(next, transform, shouldFinish)))

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

  implicit def Concatenation[A](implicit arb: Arbitrary[IO[A]]): Monoid[Timeline[A]] =
    new Monoid[Timeline[A]] {
      final override protected lazy val arbitrary: Arbitrary[Timeline[A]] =
        implicitly[Arbitrary[Timeline[A]]]

      final override lazy val operation: ClosedBinaryOperation[Timeline[A]] =
        _ #::: _

      final override lazy val uniqueIdentityElement: Timeline[A] =
        end[A]
    }

  private sealed abstract class Data[+Event]
    extends Foldable[Event]
    with (Int => Option[Event]) {
    final override def apply(index: Int): Option[Event] = {
      @scala.annotation.tailrec
      def loop(
          data: Data[Event],
          count: Int
      ): Option[Event] =
        if (index < 0)
          None
        else if (count == index)
          data.head
        else
          loop(data.tail, count + 1)

      loop(this, 0)
    }

    final def head: Option[Event] =
      this match {
        case Data.End =>
          None

        case Data.NonEmpty(recentEvent, _) =>
          Some(recentEvent.unsafeRun())
      }

    final def tail: Data[Event] =
      this match {
        case Data.End =>
          Data.End

        case Data.NonEmpty(_, followingEvents) =>
          followingEvents.unsafeRun()
      }

    final def isEmpty: Boolean =
      this.isInstanceOf[Data.End.type]

    final def nonEmpty: Boolean =
      !isEmpty

    @scala.annotation.tailrec
    final override def foldLeft[Result](seed: Result)(function: (Result, => Event) => Result): Result =
      this match {
        case Data.End =>
          seed

        case Data.NonEmpty(recentEvent, followingEvents) =>
          val currentResult = function(seed, recentEvent.unsafeRun())
          followingEvents.unsafeRun().foldLeft(currentResult)(function)
      }

    final override def foldRight[Result](seed: => Result)(function: (=> Event, => Result) => Result): Result =
      this match {
        case Data.End =>
          seed

        case Data.NonEmpty(recentEvent, followingEvents) =>
          lazy val otherResult = followingEvents.unsafeRun().foldRight(seed)(function)
          function(recentEvent.unsafeRun(), otherResult)
      }

    final def take(amount: Int): Data[Event] = {
      @scala.annotation.tailrec
      def loop(
          data: Data[Event],
          acc: Data[Event],
          count: Int
      ): Data[Event] = data match {
        case Data.End =>
          acc

        case Data.NonEmpty(recentEvent, followingEvents) =>
          lazy val nextCount =
            count + 1

          lazy val nextAcc =
            recentEvent.unsafeRun() #:: acc

          if (count >= amount)
            acc
          else if (nextCount == amount)
            nextAcc
          else
            loop(
              data  = followingEvents.unsafeRun(),
              acc   = nextAcc,
              count = nextCount
            )
      }

      loop(this, Data.End, 0).reversed
    }

    final def reversed: Data[Event] =
      foldLeft[Data[Event]](Data.End)(_ add _)

    final def filter(predicate: Event => Boolean): Data[Event] =
      foldRight[Data[Event]](Data.End) { (current, acc) =>
        val memoizedCurrent = current

        if (predicate(memoizedCurrent))
          memoizedCurrent #:: acc
        else
          acc
      }

    final def withFilter(predicate: Event => Boolean): Data[Event] =
      filter(predicate)

    final def takeWhile(predicate: Event => Boolean): Data[Event] =
      foldRight[Data[Event]](Data.End) { (current, acc) =>
        val memoizedCurrent = current

        if (predicate(memoizedCurrent))
          memoizedCurrent #:: acc
        else
          Data.End
      }

    final def map[Result](function: Event => Result): Data[Result] =
      foldRight[Data[Result]](Data.End)(function(_) #:: _)

    final def flatMap[Result](function: (=> Event) => Foldable[Result]): Data[Result] =
      foldRight[Data[Result]](Data.End) { (current, acc) =>
        function(current).foldRight(acc)(_ #:: _)
      }

    final def flatten[Result](implicit view: (=> Event) => Foldable[Result]): Data[Result] =
      foldRight[Data[Result]](Data.End) { (current, acc) =>
        view(current).foldRight(acc)(_ #:: _)
      }

    final override def equals(other: Any): Boolean =
      other match {
        case that: Data[Event] =>
          if (this.isEmpty && that.isEmpty)
            true
          else if (this.isEmpty || that.isEmpty)
            false
          else {
            val Data.NonEmpty(n1Head, n1Tail) = this
            val Data.NonEmpty(n2Head, n2Tail) = that

            n1Head.unsafeRun() == n2Head.unsafeRun() && // format: OFF
            n1Tail.unsafeRun() == n2Tail.unsafeRun() // format: ON
          }

        case _ =>
          false
      }

    final override def toString: String =
      s"Data($toStringContent)"

    private[this] def toStringContent: String =
      this match {
        case Data.End =>
          ""

        case Data.NonEmpty(recentEvent, _) =>
          s"${recentEvent.unsafeRun()}, ${Console.GREEN}...${Console.RESET}"
      }

    final def zip[ThatEvent](that: Data[ThatEvent]): Data[(Event, ThatEvent)] =
      this match {
        case Data.End =>
          Data.End

        case Data.NonEmpty(recentEvent, followingEvents) =>
          that match {
            case Data.End =>
              Data.End

            case Data.NonEmpty(thatRecentEvent, thatFollowingEvents) =>
              followingEvents
                .unsafeRun()
                .zip(thatFollowingEvents.unsafeRun())
                .after(recentEvent.unsafeRun() -> thatRecentEvent.unsafeRun())
          }
      }

    final def interleave[Super >: Event](that: Data[Super]): Data[Super] =
      this match {
        case Data.End =>
          that

        case Data.NonEmpty(recentEvent, followingEvents) =>
          recentEvent.unsafeRun() #:: that.interleave(followingEvents.unsafeRun())
      }

    final def forced: List[Event] =
      foldRight[List[Event]](List.empty)(_ :: _)

    @inline final def drained: List[Event] =
      forced

    @inline final def toList: List[Event] =
      forced
  }

  private object Data {
    final case class NonEmpty[+Event] private (
        recentEvent: IO[Event],
        followingEvents: IO[Data[Event]]
    ) extends Data[Event]

    object NonEmpty {
      def apply[Event](
          recentEvent: IO[Event],
          followingEvents: IO[Data[Event]]
      ): Data[Event] =
        new NonEmpty(
          recentEvent     = recentEvent.memoized,
          followingEvents = followingEvents.memoized
        )
    }

    case object End extends Data[Nothing]

    implicit final class DataOpsFromHGC[Event](data: => Data[Event]) {
      final def add[Super >: Event](input: => Super): Data[Super] =
        NonEmpty(
          recentEvent     = IO.pure(input),
          followingEvents = IO.pure(data)
        )

      @inline final def prepend[Super >: Event](input: => Super): Data[Super] =
        add(input)

      @inline final def after[Super >: Event](input: => Super): Data[Super] =
        add(input)

      @inline final def #::[Super >: Event](input: => Super): Data[Super] =
        add(input)

      final def addMany[Super >: Event](that: Data[Super]): Data[Super] =
        that.foldRight[Data[Super]](data)(_ #:: _)

      @inline final def prependMany[Super >: Event](that: Data[Super]): Data[Super] =
        addMany(that)

      @inline final def afterMany[Super >: Event](that: Data[Super]): Data[Super] =
        addMany(that)

      @inline final def #:::[Super >: Event](that: Data[Super]): Data[Super] =
        addMany(that)
    }

    final def apply[Event](recentEvent: => Event, followingEvents: IO[Event]*): Data[Event] =
      recentEvent #:: followingEvents.foldRight[Data[Event]](Data.End)(_.unsafeRun() #:: _)

    final def apply[Event](
        first: => Event
    ): Data[Event] =
      first #:: Data.End

    final def apply[Event](
        first: => Event,
        second: => Event
    ): Data[Event] =
      first #:: apply(second)

    final def apply[Event](
        first: => Event,
        second: => Event,
        third: => Event
    ): Data[Event] =
      first #:: apply(second, third)

    final def apply[Event](
        first: => Event,
        second: => Event,
        third: => Event,
        fourth: => Event
    ): Data[Event] =
      first #:: apply(second, third, fourth)

    final def apply[Event](
        first: => Event,
        second: => Event,
        third: => Event,
        fourth: => Event,
        fifth: => Event
    ): Data[Event] =
      first #:: apply(second, third, fourth, fifth)

    final def apply[Event](
        first: => Event,
        second: => Event,
        third: => Event,
        fourth: => Event,
        fifth: => Event,
        sixth: => Event
    ): Data[Event] =
      first #:: apply(second, third, fourth, fifth, sixth)

    final def apply[Event](
        first: => Event,
        second: => Event,
        third: => Event,
        fourth: => Event,
        fifth: => Event,
        sixth: => Event,
        seventh: => Event
    ): Data[Event] =
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
    ): Data[Event] =
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
    ): Data[Event] =
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
    ): Data[Event] =
      first #:: apply(second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth) #::: followingEvents.foldRight[Data[Event]](Data.End)(_.unsafeRun() #:: _)

    @inline final def unfold[Gen, Event](seed: => Gen)(
        next: (=> Gen) => Option[(IO[Event], IO[Gen])]
    ): Data[Event] =
      canonicalUnfold(seed)(
        next         = gen => next(gen).get._2.unsafeRun(),
        transform    = gen => next(gen).get._1.unsafeRun(),
        shouldFinish = gen => next(gen).isEmpty
      )

    final def canonicalUnfold[Gen, Event](seed: => Gen)(
        next: Gen => Gen,
        transform: Gen => Event,
        shouldFinish: (=> Gen) => Boolean
    ): Data[Event] = {
      lazy val memoizedSeed = seed

      if (shouldFinish(memoizedSeed))
        Data.End
      else if (shouldFinish(next(memoizedSeed)))
        transform(memoizedSeed) #:: Data.End
      else
        transform(memoizedSeed) #:: canonicalUnfold(next(seed))(next, transform, shouldFinish)
    }
  }
}
