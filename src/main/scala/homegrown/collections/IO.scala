package homegrown.collections

final case class IO[+A](unsafeRun: () => A) {
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
  def pure[A](a: => A): IO[A] =
    IO(() => a)
}
