import scala.util.Random

import org.scalacheck._

import homegrown.collections._

package object user {
  def bothRoles: (Employee, Consultant) =
    randomEmployee -> randomConsultant

  def randomEmployee: Employee =
    Employee(
      id = randomString
    )

  def randomConsultant: Consultant =
    Consultant(
      id          = randomString,
      companyName = randomString
    )

  def randomString: String =
    Random.alphanumeric.take(5).mkString

  def randomInt: Int =
    Random.nextInt

  implicit def arbitrarySet[T: Arbitrary]: Arbitrary[Set[T]] =
    Arbitrary(genSet[T])

  def genSet[T: Arbitrary]: Gen[Set[T]] =
    Gen.listOf(Arbitrary.arbitrary[T]).map {
      case Nil          => Set.empty[T]
      case head :: tail => Set(head, tail: _*)
    }

  def genNonEmptySet[T: Arbitrary]: Gen[Set[T]] =
    Gen.nonEmptyListOf(Arbitrary.arbitrary[T]).map {
      case Nil          => sys.error("should not happen")
      case head :: tail => Set(head, tail: _*)
    }

  implicit def arbitraryList[T: Arbitrary]: Arbitrary[List[T]] =
    Arbitrary(genList[T])

  def genList[T: Arbitrary]: Gen[List[T]] =
    Gen.listOf(Arbitrary.arbitrary[T]).map {
      case Nil          => List.empty[T]
      case head :: tail => List(head, tail: _*)
    }

  def genNonEmptyList[T: Arbitrary]: Gen[List[T]] =
    Gen.nonEmptyListOf(Arbitrary.arbitrary[T]).map {
      case Nil          => sys.error("should not happen")
      case head :: tail => List(head, tail: _*)
    }
}
