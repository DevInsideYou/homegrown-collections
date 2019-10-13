package user

import homegrown.mathlibrary._

class GroupTheorySuite extends TestSuite {
  test("Group Theory") {
    IntAddition: AbelianGroup[Int]
    IntMultiplication: Monoid[Int]

    BooleanAddition: Rig[Boolean]
    BooleanMultiplication: Rig[Boolean]

    StringConcatenation: Monoid[String]
  }
}
