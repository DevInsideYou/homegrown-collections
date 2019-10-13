package user

import org.scalacheck._

sealed trait CompanyRole {
  def id: String
  final def roleName: String = getClass.toString
}

final case class Employee(id: String) extends CompanyRole {
  final def takeVacation(): Unit = {
    println("taking a vacation")
  }
}

object Employee {
  implicit val arbitrary: Arbitrary[Employee] =
    Arbitrary(gen)

  val gen: Gen[Employee] =
    Arbitrary.arbitrary[String].map(Employee.apply)
}

final case class Consultant(id: String, companyName: String)
    extends CompanyRole {
  final def submitInvoice(): Unit = {
    println("here is my invoice")
  }
}

object Consultant {
  implicit val arbitrary: Arbitrary[Consultant] =
    Arbitrary(gen)

  val gen: Gen[Consultant] = {
    val g: Gen[String] = Arbitrary.arbitrary[String]

    for {
      id <- g
      companyName <- g
    } yield Consultant(id, companyName)
  }
}
