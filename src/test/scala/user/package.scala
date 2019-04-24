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
}
