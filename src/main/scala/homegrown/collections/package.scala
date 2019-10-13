package homegrown

package object collections {
  type Stack[+Element] = List[Element]
  @inline val Stack: List.type = List

  final implicit class InfixNotationForAnyFromHGC[Gen](seed: => Gen) {
    @inline final def io: IO[Gen] =
      IO.pure(seed)

    @inline final def timeline: Timeline[Gen] =
      Timeline.generateSame(seed)

    @inline final def timeline(next: Gen => Gen): Timeline[Gen] =
      Timeline.generate(seed)(next)

    @inline final def timelineTransform[Event](
        next: (=> Gen) => (IO[Event], IO[Gen])
      ): Timeline[Event] =
      Timeline.generateTransform(seed)(next)

    @inline final def timelineTransform[Event](
        next: Gen => Gen,
        transform: Gen => Event
      ): Timeline[Event] =
      Timeline.canonicalGenerateTransform(seed)(next, transform)

    @inline final def unfold[Event](
        next: (=> Gen) => Option[(IO[Event], IO[Gen])]
      ): Timeline[Event] =
      Timeline.unfold(seed)(next)

    @inline final def unfold[Event](
        next: Gen => Gen,
        transform: Gen => Event,
        shouldFinish: (=> Gen) => Boolean
      ): Timeline[Event] =
      Timeline.canonicalUnfold(seed)(next, transform, shouldFinish)
  }
}
