import homegrown.collections._

import org.scalatest._

class SetSuite extends FunSuite with Matchers {
  test("apply on an empty Set should yield false") {
    Set.empty(randomString) shouldBe false
  }

  test("add on an empty Set should yield a new Set with one element") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first)

    set(first) shouldBe true
    set(second) shouldBe false
  }

  test("add on a non empty Set should yieald a new Set with two elements") {
    val first = randomString
    val second = randomString

    first should not be second

    val set = Set.empty.add(first).add(second)

    set(first) shouldBe true
    set(second) shouldBe true
  }

  test("remove on a non empty Set should yield a new Set without the element") {
    val element = randomString
    val setWithElement = Set.empty.add(element)

    setWithElement(element) shouldBe true

    val setWithoutElement = setWithElement.remove(element)

    setWithoutElement(element) shouldBe false
  }

  test("remove removes only the element in question") {
    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(first)

    setWithoutElement(first) shouldBe false
    setWithoutElement(second) shouldBe true
  }

  test("remove removes only the element in question (ordering test)") {
    val first = randomString
    val second = randomString

    val setWithElement = Set.empty.add(first).add(second)

    setWithElement(first) shouldBe true
    setWithElement(second) shouldBe true

    val setWithoutElement = setWithElement.remove(second)

    setWithoutElement(first) shouldBe true
    setWithoutElement(second) shouldBe false
  }

  test("union on empty Set should yield an empty Set") {
    Set.empty.union(Set.empty)(randomString) shouldBe false
  }

  test("union on a non empty Set with an empty set should yield the original Set untouched") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(first).add(second)

    emptySet.union(nonEmptySet)(first) shouldBe true
    emptySet.union(nonEmptySet)(second) shouldBe true

    nonEmptySet.union(emptySet)(first) shouldBe true
    nonEmptySet.union(emptySet)(second) shouldBe true
  }

  test("union on two non empty Sets should yield their union") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b)
    val right = Set.empty.add(c).add(d)

    val leftUnion = left.union(right)

    leftUnion(a) shouldBe true
    leftUnion(b) shouldBe true
    leftUnion(c) shouldBe true
    leftUnion(d) shouldBe true

    val rightUnion = left.union(right)

    rightUnion(a) shouldBe true
    rightUnion(b) shouldBe true
    rightUnion(c) shouldBe true
    rightUnion(d) shouldBe true
  }

  test("intersection on empty Set should yield an empty Set") {
    Set.empty.intersection(Set.empty)(randomString) shouldBe false
  }

  test("intersection on a non empty Set with an empty Set should yield an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(first).add(second)

    emptySet.intersection(nonEmptySet)(first) shouldBe false
    emptySet.intersection(nonEmptySet)(second) shouldBe false

    nonEmptySet.intersection(emptySet)(first) shouldBe false
    nonEmptySet.intersection(emptySet)(second) shouldBe false
  }

  test("intersection on two non empty Sets should yield their intersection") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(b).add(c).add(d)

    val leftIntersection = left.intersection(right)

    leftIntersection(a) shouldBe false
    leftIntersection(b) shouldBe true
    leftIntersection(c) shouldBe true
    leftIntersection(d) shouldBe false

    val rightIntersection = left.intersection(right)

    rightIntersection(a) shouldBe false
    rightIntersection(b) shouldBe true
    rightIntersection(c) shouldBe true
    rightIntersection(d) shouldBe false
  }

  test("difference on empty Set should yield an empty Set") {
    Set.empty.difference(Set.empty)(randomString) shouldBe false
  }

  test("difference on a non empty Set with an empty Set should yield an empty Set") {
    val first = randomString
    val second = randomString

    first should not be second

    val emptySet = Set.empty
    val nonEmptySet = emptySet.add(first).add(second)

    emptySet.difference(nonEmptySet)(first) shouldBe false
    emptySet.difference(nonEmptySet)(second) shouldBe false

    nonEmptySet.difference(emptySet)(first) shouldBe true
    nonEmptySet.difference(emptySet)(second) shouldBe true
  }

  test("difference on two non empty Sets should yield their difference") {
    val a = randomString
    val b = randomString
    val c = randomString
    val d = randomString

    val left = Set.empty.add(a).add(b).add(c)
    val right = Set.empty.add(b).add(c).add(d)

    val leftDifference = left.difference(right)

    leftDifference(a) shouldBe true
    leftDifference(b) shouldBe false
    leftDifference(c) shouldBe false
    leftDifference(d) shouldBe false

    val rightDifference = right.difference(left)

    rightDifference(a) shouldBe false
    rightDifference(b) shouldBe false
    rightDifference(c) shouldBe false
    rightDifference(d) shouldBe true
  }

  test("isSubsetOf on an empty Set should yield true") {
    pending

    Set.empty.isSubsetOf(Set.empty) shouldBe true
    Set.empty.isSubsetOf(Set.empty.add(randomString)) shouldBe true
  }

  private def randomString: String =
    scala.util.Random.alphanumeric.take(5).mkString
}
