package user

import homegrown.collections._

class TreeSuite extends TestSuite {
  test("toString") {
    Tree.empty.toString shouldBe "Tree.Empty"
    Tree(1).toString shouldBe "Tree.NonEmpty(Tree.Empty,1,Tree.Empty)"
  }

  test("remove on an empty Tree should yield an empty Tree") {
    Tree.empty[String].remove(randomString) shouldBe Tree.empty[String]
  }

  test("remove on a non empty Tree should yield a new Tree without the element") {
    val element = randomString
    val treeWithElement = Tree(element)

    treeWithElement.contains(element) shouldBe true

    val treeWithoutElement = treeWithElement.remove(element)

    treeWithoutElement.contains(element) shouldBe false
  }

  test("remove removes only the element in question") {
    val first = randomString
    val second = randomString

    val treeWithElement = Tree(first, second)

    treeWithElement.contains(first) shouldBe true
    treeWithElement.contains(second) shouldBe true

    val treeWithoutElement = treeWithElement.remove(first)

    treeWithoutElement.contains(first) shouldBe false
    treeWithoutElement.contains(second) shouldBe true
  }

  test("remove removes only the element in question (ordering test)") {
    val first = randomString
    val second = randomString

    val treeWithElement = Tree(first, second)

    treeWithElement.contains(first) shouldBe true
    treeWithElement.contains(second) shouldBe true

    val treeWithoutElement = treeWithElement.remove(second)

    treeWithoutElement.contains(first) shouldBe true
    treeWithoutElement.contains(second) shouldBe false
  }

  test("add/remove combo should ensure that all elements are distinct") {
    val element = randomString

    val tree = Tree(element, element, element, element).remove(element)

    tree.contains(element) shouldBe false
  }

  test("remove should remove elements from both sides of the tree") {
    Tree(1, 2, 3).remove(3) shouldBe Tree(1, 2)
    Tree(1, -2, -3).remove(-3) shouldBe Tree(1, -2)
  }

  test("rendered") {
    testRendering(Tree.empty)("")

    testRendering(Tree(4)) {
      s"""|4
          |""".stripMargin
    }

    testRendering(Tree(4, 3)) {
      s"""|4
          |└── 3
          |""".stripMargin
    }

    testRendering(Tree(4, 3, 2)) {
      s"""|4
          |└── 3
          |    └── 2
          |""".stripMargin
    }

    testRendering(Tree(4, 3, 2, 1)) {
      s"""|4
          |└── 3
          |    └── 2
          |        └── 1
          |""".stripMargin
    }

    testRendering(Tree(1)) {
      s"""|1
          |""".stripMargin
    }

    testRendering(Tree(1, 2)) {
      s"""|1
          |├── 2
          |""".stripMargin
    }

    testRendering(Tree(1, 2, 3)) {
      s"""|1
          |├── 2
          |│   ├── 3
          |""".stripMargin
    }

    testRendering(Tree(1, 2, 3, 4)) {
      s"""|1
          |├── 2
          |│   ├── 3
          |│   │   ├── 4
          |""".stripMargin
    }

    testRendering(Tree(1, 3, 2, 4)) {
      s"""|1
          |├── 3
          |│   ├── 4
          |│   └── 2
          |""".stripMargin
    }

    testRendering(Tree(1, 3, 2, 4, -1)) {
      s"""|1
          |├── 3
          |│   ├── 4
          |│   └── 2
          |└── -1
          |""".stripMargin
    }

    testRendering(Tree(1, 3, 2, 4, -1, 0, -2)) {
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

  private def testRendering(set: Tree[_])(expected: String) = {
    val actual = set.rendered

    // println(Console.YELLOW + actual + Console.RESET)
    // println("-" * 25)
    // println(Console.GREEN + expected + Console.RESET)
    // println("-" * 50)

    actual shouldBe expected
  }
}
