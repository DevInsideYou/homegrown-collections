package homegrown.collections

sealed abstract class Trampoline[+Result] {
  import Trampoline._

  @scala.annotation.tailrec
  def result: Result = this match {
    case Done(r)      => r
    case Thunk(thunk) => thunk().result
  }
}

object Trampoline {
  private final case class Done[+Result](r: Result) extends Trampoline[Result]
  private final case class Thunk[+Result](thunk: () => Trampoline[Result]) extends Trampoline[Result]

  def done[Result](r: Result): Trampoline[Result] =
    Done(r)

  def tailcall[Result](thunk: => Trampoline[Result]): Trampoline[Result] =
    Thunk(() => thunk)
}
