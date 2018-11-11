package homegrown.collections

object Fibonacci extends App {
  println("─" * 50)

  def fibonacciOriginal(n: Long): Long =
    if (n == 0)
      0
    else if (n == 1)
      1
    else
      fibonacciOriginal(n - 1) + fibonacciOriginal(n - 2)

  def fibonacciTailRec(n: Long): Long = {
    @scala.annotation.tailrec
    def loop(x: Long, acc1: Long, acc2: Long): Long = {
      if (x == 0)
        acc1
      else if (x == 1)
        acc2
      else
        loop(
          x    = x - 1,
          acc1 = acc2,
          acc2 = acc1 + acc2
        )
    }

    loop(n, 0, 1)
  }

  def fibonacciTailRec2(n: Long): Long = {
    @scala.annotation.tailrec
    def loop(x: Long, acc2: Long, acc1: Long): Long = {
      if (x == 0)
        acc1
      else if (x == 1)
        acc2
      else
        loop(
          x    = x - 1,
          acc2 = acc1 + acc2,
          acc1 = acc2
        )
    }

    loop(n, 1, 0)
  }

  def fibonacciTailRecStackAcc(n: Long): Long = {
    @scala.annotation.tailrec
    def loop(x: Long, stack: Stack[Long]): Long = {
      val Stack.NonEmpty(acc1, // format: OFF
            Stack.NonEmpty(acc2,
              _
            )
      ) = stack // format: ON

      if (x == 0)
        acc1
      else if (x == 1)
        acc2
      else
        loop(
          x     = x - 1,
          stack = Stack.empty.push(acc1 + acc2).push(acc2)
        )
    }

    loop(n, Stack.empty.push[Long](1).push(0))
  }

  final def cps[Input, ContinuationResult](input: Input)(continuation: Input => ContinuationResult): ContinuationResult =
    continuation(input)

  def fibonacciCPSWithHelper(n: Long): Long = {
    // @scala.annotation.tailrec
    def loop(x: Long): Long = {
      if (x == 0)
        cps(x)(identity[Long])
      else if (x == 1)
        cps(x)(identity[Long])
      else
        cps(x - 2) { acc1 =>
          loop(acc1) + cps(x - 1) { acc2 =>
            loop(acc2)
          }
        }
    }

    loop(n)
  }

  def fibonacciCPS(n: Long): Long = {
    def loop(x: Long, continuation: Long => Long): Long =
      if (x == 0)
        continuation(x)
      else if (x == 1)
        continuation(x)
      else
        loop(
          x            = x - 2,
          continuation = { acc1 =>
            loop(
              x            = x - 1,
              continuation = { acc2 =>
                continuation(acc1 + acc2)
              }
            )
          }
        )

    loop(n, identity)
  }

  def fibonacciTailRecStack(n: Long): Long = {
    @scala.annotation.tailrec
    def loop(stack: Stack[Long], acc1: Long, acc2: Long): Long = {
      val x: Long = stack.peek.get

      if (x == 0)
        acc1
      else if (x == 1)
        acc2
      else
        loop(
          stack = stack.push(x - 1),
          acc1  = acc2,
          acc2  = acc1 + acc2
        )
    }

    loop(Stack.empty.push(n), 0, 1)
  }

  val fibis: Seq[Long => Long] =
    Seq(
      fibonacciOriginal,
      fibonacciTailRec,
      fibonacciTailRec2,
      fibonacciTailRecStackAcc,
      fibonacciCPSWithHelper,
      fibonacciTailRecStack,
      fibonacciCPS
    )

  def areAllElementsEqual(results: Seq[Long]): Boolean = results match {
    case Seq()                => true
    case Seq(head, tail @ _*) => tail.forall(_ == head)
  }

  @scala.annotation.tailrec
  def isEven(n: Int): Boolean = n match {
    case 0 => true
    case _ => isOdd(n - 1) || isEven(0)
  }

  @scala.annotation.tailrec
  def isOdd(n: Int): Boolean = n match {
    case 0 => false
    case _ => isEven(n - 1) || isOdd(1)
  }

  println(0 to 9 map isEven mkString "\t")
  println(0 to 9 map isOdd mkString "\t")

  // (0 to 10)
  //   .map { n =>
  //     n -> fibis.map(f => f(n))
  //   }
  //   .map {
  //     case (n, results) =>
  //       val color =
  //         if (areAllElementsEqual(results))
  //           Console.GREEN
  //         else
  //           Console.RED

  //       val row =
  //         (n +: results).mkString("\t")

  //       color + row + Console.RESET
  //   }
  //   .foreach(println)

  println("─" * 50)
}
