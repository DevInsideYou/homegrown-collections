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
    scala.util.Random.alphanumeric.take(5).mkString
}
