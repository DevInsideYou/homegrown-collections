package homegrown.collections

import Trampoline._

object WhyMe extends App {
  Set(0, 1 to 99999: _*)
}

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

  def fibonacciCPSTrampolined(n: Long): Long = {
    def loop(x: Long, continuation: Long => Trampoline[Long]): Trampoline[Long] =
      if (x == 0)
        continuation(x)
      else if (x == 1)
        continuation(x)
      else
        loop(
          x            = x - 2,
          continuation = { acc1 =>
            tailcall {
              loop(
                x            = x - 1,
                continuation = { acc2 =>
                  tailcall(continuation(acc1 + acc2))
                }
              )
            }
          }
        )

    loop(n, done).result
  }

  def fibonacciTrampolined(n: Long): Long = {
    def loop(x: Long): Trampoline[Long] =
      if (x == 0)
        done(0)
      else if (x == 1)
        done(1)
      else for {
        acc1 <- tailcall(loop(x - 2))
        acc2 <- tailcall(loop(x - 1))
      } yield acc1 + acc2

    // tailcall(loop(x - 2)).flatMap { acc1 =>
    //   tailcall(loop(x - 1)).map { acc2 =>
    //     acc1 + acc2
    //   }
    // }

    // done {
    //   tailcall(loop(x - 1)).result + tailcall(loop(x - 2)).result
    // }

    loop(n).result
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

  def factorial(n: Long): Long =
    if (n == 0)
      1
    else
      n * factorial(n - 1)

  def factorialCPS(n: Long): Long = {
    def loop(x: Long, continuation: Long => Long): Long =
      if (x == 0)
        continuation(1)
      else
        loop(
          x            = x - 1,
          continuation = { acc =>
            continuation(x * acc)
          }
        )

    loop(n, identity)
  }

  def factorialCPSTrampolined(n: Long): Long = {
    def loop(x: Long, continuation: Long => Trampoline[Long]): Trampoline[Long] =
      if (x == 0)
        continuation(1)
      else
        loop(
          x            = x - 1,
          continuation = { acc =>
            tailcall(continuation(x * acc))
          }
        )

    loop(n, done).result
  }

  def factorialTrampolined(n: Long): Long = {
    def loop(x: Long): Trampoline[Long] =
      if (x == 0)
        done(1)
      else
        tailcall(loop(x - 1)).map { acc =>
          x * acc
        }

    // done {
    //   x * tailcall(loop(x - 1)).result
    // }

    // tailcall(loop(x - 1)).flatMap { acc =>
    //   done(x * acc)
    // }

    loop(n).result
  }

  val factis: Seq[Long => Long] =
    Seq(
      // factorial,
      // factorialCPS,
      // factorialCPSTrampolined,
      factorialTrampolined
    )

  val fibis: Seq[Long => Long] =
    Seq(
      fibonacciOriginal,
      // fibonacciTailRec,
      // fibonacciTailRec2,
      // fibonacciTailRecStackAcc,
      // fibonacciCPSWithHelper,
      // fibonacciTailRecStack,
      // fibonacciCPS,
      // fibonacciCPSTrampolined,
      fibonacciTrampolined
    )

  def areAllElementsEqual(results: Seq[Long]): Boolean = results match {
    case Seq()                => true
    case Seq(head, tail @ _*) => tail.forall(_ == head)
  }

  def isEven(n: Int): Trampoline[Boolean] = n match {
    case 0 => done(true)
    case _ => tailcall(isOdd(n - 1))
  }

  def isOdd(n: Int): Trampoline[Boolean] = n match {
    case 0 => done(false)
    case _ => tailcall(isEven(n - 1))
  }

  // println(isOdd(99999).result)
  // println(isEven(99999).result)
  // println(isOdd(100000).result)
  // println(isEven(100000).result)

  // println(0 to 9 map isEven mkString "\t")
  // println(0 to 9 map isOdd mkString "\t")

  (0 to 10)
    .map { n =>
      n -> fibis.map(f => f(n))
    }
    .map {
      case (n, results) =>
        val color =
          if (areAllElementsEqual(results))
            Console.GREEN
          else
            Console.RED

        val row =
          (n +: results).mkString("\t")

        color + row + Console.RESET
    }
    .foreach(println)

  println("─" * 50)
}
