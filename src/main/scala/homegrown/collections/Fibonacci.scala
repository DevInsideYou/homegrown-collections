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

  0 to 10 map (_.toLong) map fibonacciTailRecStack foreach println

  println("─" * 50)
}
