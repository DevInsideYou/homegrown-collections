package user

import org.scalacheck._

sealed abstract class Distinct extends Product with Serializable

object Distinct {
  private def areDistinct[T](first: T, second: T, others: T*): Boolean = {
    val all: Seq[T] = first +: second +: others

    all.distinct.size == all.size
  }

  final case class Two[T](first: T, second: T) extends Distinct {
    require(areDistinct(first, second))
  }

  final case class Three[T](first: T, second: T, third: T) extends Distinct {
    require(areDistinct(first, second, third))
  }

  final case class Four[T](first: T, second: T, third: T, fourth: T) extends Distinct {
    require(areDistinct(first, second, third, fourth))
  }

  object Two {
    implicit def arbitrary[T: Arbitrary]: Arbitrary[Two[T]] =
      Arbitrary(gen)

    def gen[T: Arbitrary]: Gen[Two[T]] = {
      val g: Gen[T] = Arbitrary.arbitrary[T]

      for {
        first <- g
        second <- g
        if areDistinct(first, second)
      } yield Two(first, second)
    }
  }

  object Three {
    implicit def arbitrary[T: Arbitrary]: Arbitrary[Three[T]] =
      Arbitrary(gen)

    def gen[T: Arbitrary]: Gen[Three[T]] = {
      val g: Gen[T] = Arbitrary.arbitrary[T]

      for {
        first <- g
        second <- g
        third <- g
        if areDistinct(first, second, third)
      } yield Three(first, second, third)
    }
  }

  object Four {
    implicit def arbitrary[T: Arbitrary]: Arbitrary[Four[T]] =
      Arbitrary(gen)

    def gen[T: Arbitrary]: Gen[Four[T]] = {
      val g: Gen[T] = Arbitrary.arbitrary[T]

      for {
        first <- g
        second <- g
        third <- g
        fourth <- g
        if areDistinct(first, second, third, fourth)
      } yield Four(first, second, third, fourth)
    }
  }
}
