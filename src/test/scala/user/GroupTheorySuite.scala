package user

import homegrown.mathlibrary._

class GroupTheorySuite extends TestSuite {
  test("Group Theory") {
    val intAddition: AbelianGroup[Int] = IntAddition
    val intMultiplication: Monoid[Int] = IntMultiplication

    val booleanAddition: Rig[Boolean] = BooleanAddition
    val booleanMultiplication: Rig[Boolean] = BooleanMultiplication

    val stringConcatenation: Monoid[String] = StringConcatenation
  }
}
