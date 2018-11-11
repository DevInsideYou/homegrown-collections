package homegrown.collections

sealed abstract class Trampoline[+Result] {
  import Trampoline._

  @scala.annotation.tailrec
  def result: Result = this match {
    case Done(r) =>
      r

    case Thunk(thunk) =>
      thunk().result

    case FlatMap(trampoline, continuation) => trampoline match {
      case Done(r) =>
        continuation(r).result

      case Thunk(thunk) =>
        thunk().flatMap(continuation).result

      case FlatMap(innerTrampoline, innerContinuation) =>
        innerTrampoline.flatMap { r =>
          innerContinuation(r).flatMap(continuation)
        }.result
    }
  }

  final def map[R](continuation: Result => R): Trampoline[R] =
    flatMap(result => done(continuation(result)))

  final def flatMap[R](continuation: Result => Trampoline[R]): Trampoline[R] = this match {
    case self: FlatMap[r, _] =>
      val innerTrampoline = self.trampoline
      val innerContinuation = self.continuation

      FlatMap(innerTrampoline, { r: r =>
        innerContinuation(r).flatMap(continuation)
      })
    case trampoline => FlatMap(trampoline, continuation)
  }
}

object Trampoline {
  private final case class Done[+Result](r: Result) extends Trampoline[Result]
  private final case class Thunk[+Result](thunk: () => Trampoline[Result]) extends Trampoline[Result]
  private final case class FlatMap[R, +Result](trampoline: Trampoline[R], continuation: R => Trampoline[Result]) extends Trampoline[Result]

  def done[Result](r: Result): Trampoline[Result] =
    Done(r)

  def tailcall[Result](thunk: => Trampoline[Result]): Trampoline[Result] =
    Thunk(() => thunk)
}
