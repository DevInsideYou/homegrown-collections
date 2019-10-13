package user

import homegrown.collections._
import homegrown.mathlibrary._

final class TimelineSuiteWithMemoization extends TimelineSuite {
  IO.enableMemoization()
}

final class TimelineSuiteWithoutMemoization extends TimelineSuite {
  IO.disableMemoization()
}

sealed abstract class TimelineSuite extends TestSuite {
  test("numbers") {
    lazy val ones: Timeline[Int] =
      1 #:: ones

    ones.take(5) shouldBe Timeline(1, 1, 1, 1, 1)

    val ones2: Timeline[Int] =
      1.timeline

    ones2.take(5) shouldBe Timeline(1, 1, 1, 1, 1)

    def ints(seed: Int): Timeline[Int] =
      seed #:: ints(seed + 1)

    ints(seed = 0).take(5) shouldBe Timeline(0, 1, 2, 3, 4)

    def ints2(seed: Int): Timeline[Int] =
      seed.timeline(_ + 1)

    ints2(seed = 0).take(5) shouldBe Timeline(0, 1, 2, 3, 4)

    def ints3(seed: Int): Timeline[Int] =
      seed #:: ones.zip(ints3(seed)).map {
        case (one, acc) => acc + one
      }

    ints3(seed = 0).take(5) shouldBe Timeline(0, 1, 2, 3, 4)

    Timeline.Int.asc(from = 0).take(5) shouldBe Timeline(0, 1, 2, 3, 4)
    Timeline.Int.desc(from = 10).take(3) shouldBe Timeline(10, 9, 8)

    Timeline.Int.Range.inclusive(5, 9) shouldBe Timeline(5, 6, 7, 8, 9)
    Timeline.Int.Range.exclusive(5, 9) shouldBe Timeline(5, 6, 7, 8)

    Timeline.Int.Range.inclusive(9, 5) shouldBe Timeline.End
    Timeline.Int.Range.exclusive(9, 5) shouldBe Timeline.End
  }

  test("factorial") {
    def assert(timeline: Timeline[Int]): Unit = {
      timeline.take(10) shouldBe Timeline(1, 1, 2, 6, 24, 120, 720, 5040, 40320,
        362880)
    }

    val factorialTimeline: Timeline[Int] = {
      def recursiveFactorial(n: Int): Int = {
        @scala.annotation.tailrec
        def loop(x: Int, acc: Int): Int =
          if (x == 0)
            acc
          else
            loop(
              x = x - 1,
              acc = acc * x
            )

        loop(n, 1)
      }

      Timeline.Int.asc(from = 0).map(recursiveFactorial)
    }

    assert(factorialTimeline)

    val factorialTimeline2: Timeline[Int] = {
      def corecursiveFactorial(x: Int, acc: Int): Timeline[Int] = {
        val next = x + 1

        acc #:: corecursiveFactorial(next, acc * next)
      }

      corecursiveFactorial(x = 0, acc = 1)
    }

    assert(factorialTimeline2)

    lazy val factorialTimeline3: Timeline[Int] =
      1 #:: Timeline.Int.asc(from = 1).zip(factorialTimeline3).map {
        case (x, acc) => acc * x
      }

    assert(factorialTimeline3)
  }

  test("fibonacci") {
    def assert(timeline: Timeline[Int]): Unit = {
      timeline.take(10) shouldBe Timeline(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
    }

    val fibonacciTimeline: Timeline[Int] = {
      def recursiveFibonacci(n: Int): Int = {
        @scala.annotation.tailrec
        def loop(x: Int, acc1: Int, acc2: Int): Int =
          if (x == 0)
            acc1
          else if (x == 1)
            acc2
          else
            loop(
              x = x - 1,
              acc1 = acc2,
              acc2 = acc1 + acc2
            )

        loop(n, 0, 1)
      }

      Timeline.Int.asc(from = 0).map(recursiveFibonacci)
    }

    assert(fibonacciTimeline)

    val fibonacciTimeline2: Timeline[Int] = {
      def corecursiveFibonacci(a: Int, b: Int): Timeline[Int] =
        a #:: corecursiveFibonacci(b, a + b)

      corecursiveFibonacci(a = 0, b = 1)
    }

    assert(fibonacciTimeline2)

    lazy val fibonacciTimeline3: Timeline[Int] =
      0 #:: 1 #:: fibonacciTimeline3.zip(fibonacciTimeline3.tail).map {
        case (a, b) => a + b
      }

    assert(fibonacciTimeline3)
  }

  test("primes") {
    def assert(timeline: Timeline[Int]): Unit = {
      timeline.take(10) shouldBe Timeline(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
    }

    val primesTimeline: Timeline[Int] = {
      def recursiveIsPrime(n: Int): Boolean = {
        @scala.annotation.tailrec
        def loop(x: Int): Boolean =
          if (x == 1)
            true
          else if (n % x == 0)
            false
          else
            loop(x - 1)

        loop(n - 1)
      }

      Timeline.Int.asc(from = 2).filter(recursiveIsPrime)
    }

    assert(primesTimeline)

    lazy val primesTimeline2: Timeline[Int] = {
      def recursiveIsPrime2(n: Int): Boolean = {
        def loop(primes: Timeline[Int]): Boolean =
          primes.head.exists { p =>
            if (n < p * p)
              true
            else if (n % p == 0)
              false
            else
              loop(primes.tail)
          }

        loop(primesTimeline2)
      }

      2 #:: Timeline.Int.asc(from = 3).filter(recursiveIsPrime2)
    }

    assert(primesTimeline2)

    lazy val primesTimeline3: Timeline[Int] = {
      def sieve(timeline: Timeline[Int]): Timeline[Int] =
        timeline match {
          case Timeline.End =>
            Timeline.End

          case Timeline.NonEmpty(recentEvent, followingEvents) =>
            recentEvent.unsafeRun() #:: sieve(
              followingEvents
                .unsafeRun()
                .filter(_ % recentEvent.unsafeRun() != 0)
            )
        }

      sieve(Timeline.Int.asc(from = 2))
    }

    assert(primesTimeline3)

    new Environment {
      lazy val primesTimeline3: Timeline[Int] = {
        def sieve(timeline: Timeline[Int]): Timeline[Int] =
          timeline match {
            case Timeline.End =>
              Timeline.End

            case Timeline.NonEmpty(recentEvent, followingEvents) =>
              val memoizedRecentEvent = recentEvent.unsafeRun()

              memoizedRecentEvent #:: sieve(
                followingEvents.unsafeRun().filter(_ % memoizedRecentEvent != 0)
              )
          }

        def ints(from: Int): Timeline[Int] =
          sideEffect(from) #:: ints(from + 1)

        sieve(ints(from = 2))
      }

      assert(primesTimeline3)
      eventsOccurredShouldBe(30, 31)
    }
  }

  test("gen0") {
    new Environment {
      val zeros: Timeline[Int] =
        sideEffect(0).timeline

      val fourZeros: Timeline[Int] =
        zeros.take(4)

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        fourZeros.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 0, 0, 0)
    }

    new Environment {
      def zeros: Timeline[Int] =
        sideEffect(0).timeline

      val fourZeros: Timeline[Int] =
        zeros.take(4)

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        fourZeros.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 0, 0, 0)
    }
  }

  test("gen1") {
    new Environment {
      val ints: Timeline[Int] =
        sideEffect(0).timeline(_ + 1)

      val zeroOneTwoThree: Timeline[Int] =
        ints.take(4)

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        zeroOneTwoThree.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 1, 2, 3)
    }
  }

  test("gen2") {
    final case class BankAccount(balance: Int)

    new Environment {
      val ints: Timeline[BankAccount] =
        sideEffect(0).timelineTransform(_ + 1, BankAccount)

      val zeroOneTwoThree: Timeline[BankAccount] =
        ints.take(4)

      eventsOccurredShouldBe(0)

      val list: List[BankAccount] =
        zeroOneTwoThree.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 1, 2, 3).map(BankAccount)
    }

    new Environment {
      val ints: Timeline[BankAccount] =
        sideEffect(0).timelineTransform { n =>
          BankAccount(n).io -> (n + 1).io
        }

      val zeroOneTwoThree: Timeline[BankAccount] =
        ints.take(4)

      eventsOccurredShouldBe(0)

      val list: List[BankAccount] =
        zeroOneTwoThree.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 1, 2, 3).map(BankAccount)
    }
  }

  test("gen3") {
    final case class BankAccount(balance: Int)

    new Environment {
      val ints: Timeline[BankAccount] =
        sideEffect(0).unfold(_ + 1, BankAccount, _ => false)

      val zeroOneTwoThree: Timeline[BankAccount] =
        ints.take(4)

      eventsOccurredShouldBe(0)

      val list: List[BankAccount] =
        zeroOneTwoThree.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 1, 2, 3).map(BankAccount)
    }

    new Environment {
      val ints: Timeline[BankAccount] =
        sideEffect(0).unfold(_ + 1, BankAccount, _ == 4)

      val zeroOneTwoThree: Timeline[BankAccount] =
        ints //.take(4)

      eventsOccurredShouldBe(0)

      val list: List[BankAccount] =
        zeroOneTwoThree.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 1, 2, 3).map(BankAccount)
    }

    new Environment {
      val ints: Timeline[BankAccount] =
        sideEffect(0).unfold { n =>
          if (n == 4)
            None
          else
            Some(BankAccount(n).io -> (n + 1).io)
        }

      val zeroOneTwoThree: Timeline[BankAccount] =
        ints //.take(4)

      eventsOccurredShouldBe(0)

      val list: List[BankAccount] =
        zeroOneTwoThree.forced

      eventsOccurredShouldBe(4)

      list shouldBe List(0, 1, 2, 3).map(BankAccount)
    }
  }

  test("gen4") {
    new Environment {
      val timeline1: Timeline[Int] =
        sideEffect(0).unfold(n => Some(n.io -> n.io))

      val timeline2: Timeline[Int] =
        sideEffect(10).unfold(n => Some(n.io -> n.io))

      val result: Timeline[Int] =
        timeline1.interleave(timeline2).take(2)

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        result.forced

      eventsOccurredShouldBe(2)

      list shouldBe List(0, 10)
    }

    new Environment {
      val timeline1: Timeline[Int] =
        sideEffect(0).unfold(
          next = identity,
          transform = identity,
          shouldFinish = _ => false
        )

      val timeline2: Timeline[Int] =
        sideEffect(10).unfold(
          next = identity,
          transform = identity,
          shouldFinish = _ => false
        )

      val result: Timeline[Int] =
        timeline1.interleave(timeline2).take(2)

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        result.forced

      eventsOccurredShouldBe(2)

      list shouldBe List(0, 10)
    }
  }

  test("init1") {
    new Environment {
      val timeline: Timeline[Int] =
        Timeline.NonEmpty(
          recentEvent = IO.pure(sideEffect(0)),
          followingEvents = IO.pure(Timeline.End)
        )

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        timeline.drained

      eventsOccurredShouldBe(1)

      list shouldBe List(0)
    }
  }

  test("init2") {
    new Environment {
      testHead {
        Timeline.NonEmpty(
          recentEvent = IO.pure(sideEffect(0)),
          followingEvents = IO.pure(
            Timeline.NonEmpty(
              recentEvent = IO.pure(sideEffect(1)),
              followingEvents = IO.pure(
                Timeline.NonEmpty(
                  recentEvent = IO.pure(sideEffect(2)),
                  followingEvents = IO.pure(Timeline.End)
                )
              )
            )
          )
        )
      }
    }
  }

  test("init3") {
    new Environment {
      testHead {
        Timeline(
          sideEffect(0),
          sideEffect(1),
          sideEffect(2)
        )
      }
    }

    new Environment {
      testHead {
        Timeline.End
          .add(sideEffect(2))
          .add(sideEffect(1))
          .add(sideEffect(0))
      }
    }

    new Environment {
      testHead {
        Timeline.End
          .prepend(sideEffect(2))
          .prepend(sideEffect(1))
          .prepend(sideEffect(0))
      }
    }

    new Environment {
      testHead {
        Timeline.End
          .after(sideEffect(2))
          .after(sideEffect(1))
          .after(sideEffect(0))
      }
    }

    new Environment {
      testHead {
        Timeline.End
          .#::(sideEffect(2))
          .#::(sideEffect(1))
          .#::(sideEffect(0))
      }
    }

    new Environment {
      testHead {
        sideEffect(0) #:: sideEffect(1) #:: sideEffect(2) #:: Timeline.End
      }
    }
  }

  test("init4") {
    new Environment {
      def ascending(seed: Int): Timeline[Int] =
        sideEffect(seed) #:: ascending(seed + 1)

      ascending(seed = 0)

      eventsOccurredShouldBe(0)
    }
  }

  test("addMany1") {
    new Environment {
      val left: Timeline[Int] =
        zeroOneTwo

      val right: Timeline[Int] =
        Timeline(sideEffect(3), sideEffect(4), sideEffect(5))

      val combined1: Timeline[Int] =
        right.addMany(left)

      val combined2: Timeline[Int] =
        right.prependMany(left)

      val combined3: Timeline[Int] =
        right.#:::(left)

      val combined4: Timeline[Int] =
        left #::: right

      eventsOccurredShouldBe(0)

      val expected: List[Int] =
        List(0, 1, 2, 3, 4, 5)

      combined1.forced shouldBe expected
      combined2.forced shouldBe expected
      combined3.forced shouldBe expected
      combined4.forced shouldBe expected
    }
  }

  test("addMany2") {
    new Environment {
      def ascending(seed: Int): Timeline[Int] =
        Timeline(sideEffect(seed)) #::: ascending(seed + 1)

      ascending(seed = 0)

      eventsOccurredShouldBe(0)
    }
  }

  test("take") {
    new Environment {
      val timeline: Timeline[Int] =
        zeroOneTwo

      val t: Timeline[Int] = timeline.take(-1)
      val t0: Timeline[Int] = timeline.take(0)
      val t1: Timeline[Int] = timeline.take(1)
      val t2: Timeline[Int] = timeline.take(2)
      val t3: Timeline[Int] = timeline.take(3)
      timeline.take(4)
      eventsOccurredShouldBe(0)

      t.forced shouldBe List.empty
      t0.forced shouldBe List.empty
      eventsOccurredShouldBe(0)

      t1.forced shouldBe List(0)
      eventsOccurredShouldBe(1)

      t2.forced shouldBe List(0, 1)
      eventsOccurredShouldBe(2, 3)

      t3.forced shouldBe List(0, 1, 2)
      eventsOccurredShouldBe(3, 6)

      t3.forced shouldBe List(0, 1, 2)
      eventsOccurredShouldBe(3, 9)

      timeline.forced shouldBe List(0, 1, 2)
      eventsOccurredShouldBe(3, 12)
    }
  }

  test("reversed") {
    new Environment {
      val timeline: Timeline[Int] =
        zeroOneTwo

      val reversed: Timeline[Int] =
        timeline.reversed

      eventsOccurredShouldBe(0)

      val list: List[Int] =
        reversed.forced

      eventsOccurredShouldBe(3)

      list shouldBe List(2, 1, 0)
    }

    new Environment {
      def ascending(seed: Int): Timeline[Int] =
        Timeline(sideEffect(seed)) #::: ascending(seed + 1)

      val ints: Timeline[Int] =
        ascending(seed = 0)

      val reversed: Timeline[Int] =
        ints.take(3).reversed

      eventsOccurredShouldBe(0)

      reversed.forced shouldBe List(2, 1, 0)
    }
  }

  test("apply") {
    new Environment {
      val timeline: Timeline[String] =
        zeroOneTwo.map(_.toString)

      timeline(-1) shouldBe None
      eventsOccurredShouldBe(0)

      0 until timeline.size foreach { index =>
        timeline(index) shouldBe Some(index.toString)
        eventsOccurredShouldBe(index + 1)
      }

      timeline(3) shouldBe None
      eventsOccurredShouldBe(3)
    }
  }

  test("toString") {
    new Environment {
      val timeline: Timeline[Int] =
        zeroOneTwo

      eventsOccurredShouldBe(0)

      val tail = Console.GREEN + "..." + Console.RESET

      timeline.toString shouldBe s"Timeline(0, $tail)"
      eventsOccurredShouldBe(1)

      timeline.tail.toString shouldBe s"Timeline(1, $tail)"
      eventsOccurredShouldBe(2)
    }
  }

  test("equality1") {
    new Environment {
      val timeline: Timeline[Int] =
        zeroOneTwo

      timeline should not be Timeline.End
      eventsOccurredShouldBe(0)

      Timeline.End should not be timeline
      eventsOccurredShouldBe(0)
    }
  }

  test("equality2") {
    val a: Timeline[Int] =
      Timeline(0, 1)

    val b: Timeline[Int] =
      Timeline(0)

    a should not be b
    b should not be a
  }

  test("equality3") {
    new Environment {
      val a: Timeline[Int] =
        zeroOneTwo

      val b: Timeline[Int] =
        zeroOneTwo

      eventsOccurredShouldBe(0)
      a shouldBe b
      eventsOccurredShouldBe(6)
    }
  }

  test("equality4") {
    new Environment {
      forAll { timeline: Timeline[Int] =>
        eventsOccurredShouldBe(0)

        val a = timeline
        val b = timeline.map(identity)

        eventsOccurredShouldBe(0)

        a shouldBe b

        whenever(timeline.nonEmpty) {
          eventsOccurredShouldNotBe(0)
        }

        resetEventsOccurredCounter()
      }
    }
  }

  test("zip1") {
    new Environment {
      val a: Timeline[Int] =
        Timeline.End

      val b: Timeline[String] =
        Timeline.End

      a zip b shouldBe Timeline.End
    }
  }

  test("zip2") {
    new Environment {
      val timeline: Timeline[(Int, String)] =
        zeroOneTwo.zip(zeroOneTwo.map(_.toString))

      eventsOccurredShouldBe(0)

      timeline.take(1).forced shouldBe List(
        0 -> "0"
      )

      eventsOccurredShouldBe(2)

      timeline.take(2).forced shouldBe List(
        0 -> "0",
        1 -> "1"
      )
      eventsOccurredShouldBe(4, 6)

      timeline.forced shouldBe List(
        0 -> "0",
        1 -> "1",
        2 -> "2"
      )

      eventsOccurredShouldBe(6, 12)
    }
  }

  test("zip3") {
    new Environment {
      val a: Timeline[Int] =
        Timeline(
          sideEffect(0),
          sideEffect(1)
        )

      val b: Timeline[String] =
        Timeline(
          sideEffect("dev"),
          sideEffect("inside"),
          sideEffect("you")
        )

      val timeline: Timeline[(Int, String)] =
        a zip b

      eventsOccurredShouldBe(0)

      timeline shouldBe Timeline(
        0 -> "dev",
        1 -> "inside"
      )

      timeline.forced shouldBe List(
        0 -> "dev",
        1 -> "inside"
      )
    }
  }

  test("zip4") {
    new Environment {
      val a: Timeline[Int] =
        Timeline(
          sideEffect(0),
          sideEffect(1),
          sideEffect(2)
        )

      val b: Timeline[String] =
        Timeline(
          sideEffect("dev"),
          sideEffect("inside")
        )

      val timeline: Timeline[(Int, String)] =
        a zip b

      eventsOccurredShouldBe(0)

      timeline shouldBe Timeline(
        0 -> "dev",
        1 -> "inside"
      )

      timeline.forced shouldBe List(
        0 -> "dev",
        1 -> "inside"
      )
    }
  }

  test("interleave1") {
    new Environment {
      val a: Timeline[Int] =
        Timeline.End

      val b: Timeline[Int] =
        Timeline.End

      a.interleave(b) shouldBe Timeline.End
    }
  }

  test("interleave2") {
    new Environment {
      val a: Timeline[Int] =
        zeroOneTwo

      val b: Timeline[Int] =
        zeroOneTwo.map(_ + 10)

      val timeline: Timeline[Int] =
        a.interleave(b)

      eventsOccurredShouldBe(0)

      timeline shouldBe Timeline(0, 10, 1, 11, 2, 12)
    }
  }

  test("interleave3") {
    new Environment {
      val a: Timeline[Int] =
        Timeline(
          sideEffect(0),
          sideEffect(1),
          sideEffect(2),
          sideEffect(3)
        )

      val b: Timeline[Int] =
        Timeline(
          10
        )

      val timeline: Timeline[Int] =
        a.interleave(b)

      eventsOccurredShouldBe(0)

      timeline shouldBe Timeline(0, 10, 1, 2, 3)
    }
  }

  test("interleave4") {
    new Environment {
      val a: Timeline[Int] =
        Timeline(
          sideEffect(0)
        )

      val b: Timeline[Int] =
        Timeline(
          sideEffect(10),
          sideEffect(11),
          sideEffect(12),
          sideEffect(13)
        )

      val timeline: Timeline[Int] =
        a.interleave(b)

      eventsOccurredShouldBe(0)

      timeline shouldBe Timeline(0, 10, 11, 12, 13)
    }
  }

  test("flat") {
    new Environment {
      forAll { inner: Timeline[Int] =>
        val outer: Timeline[Timeline[Int]] =
          Timeline(inner, inner, inner)

        outer.flatten shouldBe outer.flatMap(event => event)
      }
    }
  }

  test("filter") {
    new Environment {
      def ascending(seed: Int): Timeline[Int] =
        sideEffect(seed) #:: ascending(seed + 1)

      val ints: Timeline[Int] =
        ascending(seed = 0)

      val filtered: Timeline[Int] =
        ints
          .filter(_ % 2 == 0)
          .take(5)

      eventsOccurredShouldBe(0)

      filtered.forced shouldBe List(0, 2, 4, 6, 8)
    }
  }

  test("takeWhile") {
    new Environment {
      val timeline: Timeline[Int] =
        Timeline(
          sideEffect(1),
          sideEffect(2),
          sideEffect(3),
          sideEffect(4),
          sideEffect(5),
          sideEffect(6)
        )

      // format: OFF
      val t1 = timeline.takeWhile(_  % 2 != 0)
      val t2 = timeline.takeWhile(_  < 3)
      val t3 = timeline.takeWhile(_ <= 3)
      // format: ON

      t1.forced shouldBe List(1)
      t2.forced shouldBe List(1, 2)
      t3.forced shouldBe List(1, 2, 3)
    }
  }

  test("find") {
    val timeline: Timeline[Int] =
      Timeline(1, 2, 3, 4, 5, 6)

    // format: OFF
    timeline.find(_  % 2 != 0) shouldBe Some(1)
    timeline.find(_  % 2 == 0) shouldBe Some(2)
    timeline.find(_  < 3     ) shouldBe Some(1)
    timeline.find(_ <= 3     ) shouldBe Some(1)
    timeline.find(_ >= 7     ) shouldBe None
    // format: ON
  }

  test("aggregated1") {
    new Environment {
      def ascending(seed: Int): Timeline[Int] =
        sideEffect(seed) #:: ascending(seed + 1)

      val ints: Timeline[Int] =
        ascending(seed = 0)

      val zeroOneTwoThree: Timeline[Int] =
        ints.take(4)

      eventsOccurredShouldBe(0)

      zeroOneTwoThree.aggregated shouldBe 6
    }
  }

  test("aggregated2") {
    new Environment {
      def ascending(seed: Int): Timeline[Int] =
        sideEffect(seed) #:: ascending(seed + 1)

      val ints: Timeline[Int] =
        ascending(seed = 0)

      val threeFourFiveSix: Timeline[Int] =
        ints
          .filter(_ >= 3)
          .take(4)

      val sevenEightNine: Timeline[Int] =
        ints
          .filter(_ >= 7)
          .take(3)

      eventsOccurredShouldBe(0)

      val timeline: Timeline[Timeline[Int]] =
        Timeline(
          sideEffect(zeroOneTwo),
          sideEffect(threeFourFiveSix),
          sideEffect(sevenEightNine)
        )

      eventsOccurredShouldBe(0)

      val flattened: Timeline[Int] =
        timeline.aggregated

      // eventsOccurredShouldBe(0)

      flattened.forced shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      timeline.flatten.forced shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    }
  }

  test("reduce") {
    new Environment {
      lazy val a: Boolean = sideEffect(true)
      lazy val b: Boolean = sideEffect(false)
      lazy val c: Boolean = sideEffect(false)

      // format: OFF
      a ||
      b ||
      c shouldBe true
      // format: ON

      eventsOccurredShouldBe(1)
    }
  }

  test("reduce2") {
    new Environment {
      lazy val a: Boolean = sideEffect(true)
      lazy val b: Boolean = sideEffect(false)
      lazy val c: Boolean = sideEffect(false)

      val timeline: Timeline[Boolean] =
        Timeline(
          a,
          b,
          c
        )

      timeline.foldLeft(false)(_ || _) shouldBe true
      eventsOccurredShouldBe(1)

      timeline.aggregated(BooleanAddition) shouldBe true

      timeline.reduceLeft(_ || _) shouldBe Some(true)
      timeline.reduceLeftOrThrowException(_ || _) shouldBe true
    }
  }

  test("reduce3") {
    new Environment {
      lazy val a: Boolean = sideEffect(true)
      lazy val b: Boolean = sideEffect(false)
      lazy val c: Boolean = sideEffect(false)

      val list: List[Boolean] =
        List(
          a,
          b,
          c
        )

      list.foldLeft(false)(_ || _) shouldBe true
      // eventsOccurredShouldBe(1)

      list.aggregated(BooleanAddition) shouldBe true

      list.reduceLeft(_ || _) shouldBe Some(true)
      list.reduceLeftOrThrowException(_ || _) shouldBe true
    }
  }

  test("unapply1") {
    new Environment {
      zeroOneTwo should matchPattern { // format: OFF
        case Timeline.NonEmpty(recentEvent, followingEvents)
         if recentEvent.unsafeRun()      ==      0  &&
         followingEvents.unsafeRun().head == Some(1) =>
      } // format: ON
    }
  }

  test("unapply2") {
    new Environment {
      zeroOneTwo should matchPattern {
        case Timeline.NonEmpty(_, _) =>
      }

      eventsOccurredShouldBe(0)
    }
  }

  test("unapply3") {
    new Environment {
      zeroOneTwo should matchPattern {
        case Timeline(0, 1, 2) =>
      }

      eventsOccurredShouldBe(3)
    }
  }

  abstract class Environment {
    private[this] var eventsOccurred: Int = 0

    def resetEventsOccurredCounter(): Unit = {
      eventsOccurred = 0
    }

    def eventsOccurredShouldBe(expected: Int): Unit = {
      eventsOccurredShouldBe(expected, expected)
    }

    def eventsOccurredShouldBe(
        whenMemoized: Int,
        whenNotMemoized: Int
      ): Unit = {
      val expected: Int =
        if (IO.isMemoizationEnabled)
          whenMemoized
        else
          whenNotMemoized

      eventsOccurred shouldBe expected
    }

    def eventsOccurredShouldNotBe(expected: Int): Unit = {
      eventsOccurredShouldNotBe(expected, expected)
    }

    def eventsOccurredShouldNotBe(
        whenMemoized: Int,
        whenNotMemoized: Int
      ): Unit = {
      val expected: Int =
        if (IO.isMemoizationEnabled)
          whenMemoized
        else
          whenNotMemoized

      eventsOccurred should not be expected
    }

    def sideEffect[Event](event: Event): Event = {
      eventsOccurred += 1

      event
    }

    def testHead[Event](timeline: Timeline[Event]): Unit = {
      eventsOccurredShouldBe(0)

      timeline.head
      timeline.head
      eventsOccurredShouldBe(1, 2)

      timeline.tail.head
      timeline.tail.head
      eventsOccurredShouldBe(2, 4)

      timeline.tail.tail.head
      timeline.tail.tail.head
      eventsOccurredShouldBe(3, 6)
    }

    def zeroOneTwo: Timeline[Int] =
      Timeline(sideEffect(0), sideEffect(1), sideEffect(2))

    implicit val arbitrary: Arbitrary[IO[Int]] =
      Arbitrary(gen)

    val gen: Gen[IO[Int]] =
      Arbitrary
        .arbitrary[Int]
        .map(n => IO.pure(sideEffect(n)))
  }
}
