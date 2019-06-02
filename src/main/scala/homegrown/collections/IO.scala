package homegrown.collections

final case class IO[+A](unsafeRun: () => A) {
  import IO._

  lazy val memoized: IO[A] = {
    if (isMemoizationDisabled)
      this
    else {
      lazy val a = unsafeRun()

      IO.pure(a)
    }
  }

  def map[B](ab: A => B): IO[B] =
    IO.pure {
      val a = unsafeRun()
      val b = ab(a)

      b
    }

  def flatMap[B](aiob: A => IO[B]): IO[B] =
    IO.pure {
      val a = unsafeRun()
      val iob = aiob(a)
      val b = iob.unsafeRun()

      b
    }
}

object IO {
  def isMemoizationDisabled: Boolean =
    !isMemoizationEnabled

  def isMemoizationEnabled: Boolean =
    _isMemoizationEnabled

  private[this] var _isMemoizationEnabled =
    false

  def enableMemoization(): Unit = {
    _isMemoizationEnabled = true
  }

  def disableMemoization(): Unit = {
    _isMemoizationEnabled = false
  }

  def pure[A](a: => A): IO[A] =
    IO(() => a)
}
