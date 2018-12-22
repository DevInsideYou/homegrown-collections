package user

import homegrown.collections._

import org.scalatest._

class TreeSuite extends FunSuite with Matchers {
  test("rendered") {
    testRendering(Set.empty)("")

    testRendering(Set(4)) {
      s"""|4
          |""".stripMargin
    }

    testRendering(Set(4, 3)) {
      s"""|4
          |└── 3
          |""".stripMargin
    }

    testRendering(Set(4, 3, 2)) {
      s"""|4
          |└── 3
          |    └── 2
          |""".stripMargin
    }

    testRendering(Set(4, 3, 2, 1)) {
      s"""|4
          |└── 3
          |    └── 2
          |        └── 1
          |""".stripMargin
    }

    testRendering(Set(1)) {
      s"""|1
          |""".stripMargin
    }

    testRendering(Set(1, 2)) {
      s"""|1
          |├── 2
          |""".stripMargin
    }

    testRendering(Set(1, 2, 3)) {
      s"""|1
          |├── 2
          |│   ├── 3
          |""".stripMargin
    }

    testRendering(Set(1, 2, 3, 4)) {
      s"""|1
          |├── 2
          |│   ├── 3
          |│   │   ├── 4
          |""".stripMargin
    }

    testRendering(Set(1, 3, 2, 4)) {
      s"""|1
          |├── 3
          |│   ├── 4
          |│   └── 2
          |""".stripMargin
    }

    testRendering(Set(1, 3, 2, 4, -1)) {
      s"""|1
          |├── 3
          |│   ├── 4
          |│   └── 2
          |└── -1
          |""".stripMargin
    }

    testRendering(Set(1, 3, 2, 4, -1, 0, -2)) {
      s"""|1
          |├── 3
          |│   ├── 4
          |│   └── 2
          |└── -1
          |    ├── 0
          |    └── -2
          |""".stripMargin
    }
  }

  private def testRendering(set: Set[_])(expected: String) = {
    val actual = set.rendered

    // println(Console.YELLOW + actual + Console.RESET)
    // println("-" * 25)
    // println(Console.GREEN + expected + Console.RESET)
    // println("-" * 50)

    actual shouldBe expected
  }
}
