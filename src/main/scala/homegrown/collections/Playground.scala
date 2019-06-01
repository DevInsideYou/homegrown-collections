package homegrown.collections

import scala.io.StdIn.readLine

object Playground extends App {
  print(Console.RED)

  def runTimeline(timeline: Timeline[Any]): Unit = {
    run(timeline.drained)
  }

  def runIO(io: IO[Any]): Unit = {
    run(io.unsafeRun())
  }

  def run(code: => Any): Unit = {
    print(Console.GREEN)
    println("─" * 50)

    code

    println("─" * 50)
    print(Console.RESET)
  }

  // run {
  //   println("What's your name?")
  //   val name: String = "Vlad"
  //   println(s"My name is $name.")
  // }

  // run {
  //   var loopedTimes = 0
  //   while (loopedTimes < 2) {
  //     println("What's your name?")
  //     val name: String = "Vlad"
  //     println(s"My name is $name.")

  //     loopedTimes += 1
  //   }
  // }

  // runIO {
  //   val read: IO[String] =
  //     IO.pure("Vlad")

  //   def write(input: => Any): IO[Unit] =
  //     IO.pure(println(input))

  //   def loop(loopedTimes: Int): IO[Unit] =
  //     for {
  //       _ <- write("What's your name?")
  //       name <- read
  //       _ <- write(s"My name is $name.")
  //       _ <- if (loopedTimes == 2) IO.pure(()) else loop(loopedTimes + 1)
  //     } yield ()

  //   loop(1)
  // }

  // runTimeline {
  //   val read: Timeline[String] =
  //     Timeline.NonEmpty(
  //       recentEvent    = IO.pure("Vlad"),
  //       previousEvents = IO.pure(Timeline.End)
  //     )

  //   def write(input: => Any): IO[Unit] =
  //     IO.pure(println(input))

  //   lazy val timeline: Timeline[Unit] =
  //     Timeline.NonEmpty(
  //       recentEvent    = write("What's your name?"),
  //       previousEvents = IO.pure {
  //         read match {
  //           case Timeline.End =>
  //             Timeline.End

  //           case Timeline.NonEmpty(recentEvent, _) =>
  //             Timeline.NonEmpty(
  //               recentEvent    = write(s"My name is ${recentEvent.unsafeRun()}."),
  //               previousEvents = IO.pure(Timeline.End)
  //             )
  //         }
  //       }
  //     )

  //   timeline.take(4)
  // }

  // runTimeline {
  //   lazy val read: Timeline[String] =
  //     readLine() #:: read

  //   def write(input: Timeline[String]): Timeline[Unit] =
  //     input
  //       .map(name => s"My name is $name.")
  //       .map(println)

  //   lazy val timeline: Timeline[Unit] =
  //     println("What's your name?") #:: write(read).take(1) #::: timeline

  //   timeline.take(4)
  // }

  runTimeline {
    lazy val timeline: Timeline[Unit] =
      questions
        .interleave(alternativeQuestions)
        .map(println)
        .zip(read)
        .map {
          case (_, name) => name
        }
        .map(name => s"My name is $name.")
        .map(println) #::: timeline

    lazy val questions: Timeline[String] =
      "What's your name?" #:: questions

    lazy val alternativeQuestions: Timeline[String] =
      "What's your full name?" #:: alternativeQuestions

    lazy val read: Timeline[String] =
      readLine() #:: read

    timeline.take(4)
  }
}
