package user

import homegrown.collections._

import org.scalatest._

class MapSuite extends FunSuite with Matchers {
  test("apply on an empty Map should yield None") {
    Map.empty(randomString) shouldBe None // good that we moved randomString to the package object
    Map.empty.keys.size shouldBe 0
  }

  test("add on an empty Map should yield a new Map with one element") {
    val first = randomString
    val second = randomString

    val map = Map(first -> second)

    map(first) shouldBe Some(second)
    map(second) shouldBe None
  }

  test("add on a non empty Map should yield a new Map with two elements") {
    val first = randomString
    val second = randomString

    first should not be second

    val map =
      Map(
        first -> second,
        second -> first
      )

    map(first) shouldBe Some(second)
    map(second) shouldBe Some(first)
  }

  test("adding the same element twice should replace the value") {
    val first = randomString
    val second = randomString

    first should not be second

    val firstMap =
      Map(
        first -> second,
        second -> first,
        second -> second
      )

    firstMap(first) shouldBe Some(second)
    firstMap(second) shouldBe Some(second)

    val secondMap =
      Map(
        first -> second,
        second -> first,
        first -> first
      )

    secondMap(first) shouldBe Some(first)
    secondMap(second) shouldBe Some(first)
  }

  test("remove on an empty Map should yield an empty Map") {
    val element = randomString
    val stillEmpty = Map.empty.remove(element)
    stillEmpty(element) shouldBe None
  }

  test("remove on a non empty Map should yield a new Map without the element") {
    val key = randomString
    val value = randomString
    val mapWithElement = Map(key -> value)

    mapWithElement(key) shouldBe Some(value)

    val mapWithoutElement = mapWithElement.remove(key)

    mapWithoutElement(key) shouldBe None
  }

  test("remove removes only the element in question") {
    val first = randomString
    val second = randomString

    val mapWithElement =
      Map(
        first -> second,
        second -> first
      )

    mapWithElement(first) shouldBe Some(second)
    mapWithElement(second) shouldBe Some(first)

    val mapWithoutElement = mapWithElement.remove(first)

    mapWithoutElement(first) shouldBe None
    mapWithoutElement(second) shouldBe Some(first)
  }

  test("remove removes only the element in question (ordering test)") {
    val first = randomString
    val second = randomString

    val mapWithElement =
      Map(
        first -> second,
        second -> first
      )

    mapWithElement(first) shouldBe Some(second)
    mapWithElement(second) shouldBe Some(first)

    val mapWithoutElement = mapWithElement.remove(second)

    mapWithoutElement(first) shouldBe Some(second)
    mapWithoutElement(second) shouldBe None
  }

  test("add/remove combo should ensure that all elements are distinct") {
    val key = randomString

    val map =
      Map(
        key -> randomString,
        key -> randomString
      ).remove(key)

    map(key) shouldBe None
    map.keys.size shouldBe 0
  }

  test("remove should remove elements from both sides of the tree") {
    val value = randomString

    val firstMap =
      Map(
        1 -> value,
        2 -> value,
        3 -> value
      ).remove(3)

    firstMap(1) shouldBe Some(value)
    firstMap(2) shouldBe Some(value)
    firstMap(3) shouldBe None

    val secondMap =
      Map(
        1 -> value,
        -2 -> value,
        -3 -> value
      ).remove(-3)

    secondMap(1) shouldBe Some(value)
    secondMap(-2) shouldBe Some(value)
    secondMap(-3) shouldBe None
  }

  test("Map should be a Function1") {
    val f: String => Option[Int] =
      Map(
        "alice" -> 5,
        "bob" -> 3,
        "frank" -> 5
      )

    f("alice") shouldBe Some(5)
    f("bob") shouldBe Some(3)
    f("frank") shouldBe Some(5)
    f("eric") shouldBe None
  }

  test("keys should be public") {
    Map.empty[Int, String].keys shouldBe Set.empty[Int]
  }

  test("values should yield a Set of values (the co-domain)") {
    Map(
      1 -> 10,
      2 -> 20
    ).values shouldBe Set(10, 20)
  }

  test("isEmpty on an empty Map should yield true") {
    Map.empty.isEmpty shouldBe true
    Map.empty.nonEmpty shouldBe false
  }

  test("isEmpty on a non empty Map should yield false") {
    Map(randomString -> randomString).isEmpty shouldBe false
    Map(randomString -> randomString).nonEmpty shouldBe true
  }

  test("isSingleton on an empty Map should yield false") {
    Map.empty.isSingleton shouldBe false
  }

  test("isSingleton on a Map with more than one element should yield false") {
    val first = randomString
    val second = randomString

    first should not be second

    Map(
      first -> second,
      second -> first
    ).isSingleton shouldBe false
  }

  test("isSingleton on a Map with a single element should yield true") {
    Set(randomString).isSingleton shouldBe true
  }

  test("sample should yield a random element from the Map") {
    Map.empty.sample shouldBe None

    val a = randomString
    val b = randomString
    Map.empty.add(a -> b).sample shouldBe Some(a -> b)

    Map(
      a -> b,
      b -> a
    ).sample should contain oneOf (a -> b, b -> a)
  }

  test("size on an empty Map should be 0") {
    Map.empty.size shouldBe 0
  }

  test("size on a non empty Map should be 1") {
    Map(randomString -> randomString).size shouldBe 1
  }

  test("size on a non empty Map with 2 distinct elements added should be 2") {
    val first = randomString
    val second = randomString

    first should not be second

    Map(
      first -> second,
      second -> first
    ).size shouldBe 2
  }

  test("size on a non empty Map with 2 equal elements added should be 1") {
    val key = randomString

    Map(
      key -> randomString,
      key -> randomString
    ).size shouldBe 1
  }

  test("foreach on an empty Map should not apply the function") {
    noException should be thrownBy Map.empty[Nothing, Nothing].foreach(_ => sys.error("should not be thrown"))
  }

  test("foreach on a non empty Map should apply the function") {
    var functionWasApplied = false

    Map(randomString -> randomString).foreach(_ => functionWasApplied = true)

    functionWasApplied shouldBe true
  }

  test("foreach should be able to calculate the size of the given set 0") {
    var size = 0

    val map = Map.empty

    map.foreach(_ => size += 1)

    size shouldBe 0
    size shouldBe map.size
  }

  test("foreach should be able to calculate the size of the given set 1") {
    var size = 0

    val map = Map(randomString -> randomString)

    map.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe map.size
  }

  test("foreach should be able to calculate the size of the given set 2") {
    var size = 0

    val first = randomString
    val second = randomString

    first should not be second

    val map =
      Map(
        first -> second,
        second -> first
      )

    map.foreach(_ => size += 1)

    size shouldBe 2
    size shouldBe map.size
  }

  test("foreach should be able to calculate the size of the given set 3") {
    var size = 0

    val key = randomString

    val map =
      Map(
        key -> randomString,
        key -> randomString
      )

    map.foreach(_ => size += 1)

    size shouldBe 1
    size shouldBe map.size
  }

  test("foreach should be parameterized in the result of the argument function so that it does not produce warnings") {
    Map.empty[Nothing, Nothing].foreach(_ => 1)
  }

  test("contains on an empty Map should yield false") {
    Map.empty.contains(randomString) shouldBe false
    Map.empty.doesNotContain(randomString) shouldBe true
  }

  test("exists on an empty Map should yield false") {
    Map.empty[Nothing, Nothing].exists(_ => false) shouldBe false
    Map.empty[Nothing, Nothing].doesNotExist(_ => false) shouldBe true
  }

  test("exists on a non empty Map should yield true") {
    val element = randomString

    Map(element -> randomString).exists(_._1.size == element.size) shouldBe true
    Map(element -> randomString).exists(_._1.size != element.size) shouldBe false

    Map(element -> randomString).doesNotExist(_._1.size == element.size) shouldBe false
    Map(element -> randomString).doesNotExist(_._1.size != element.size) shouldBe true
  }

  test("exists with variance") {
    val (employee, consultant) = bothRoles

    Map(employee -> employee).exists(_._2 == employee) shouldBe true
    // Map(employee -> employee).exists(_._2 == consultant) shouldBe false // compiles :( with a warning :)

    Map[Employee, Employee](employee -> employee).exists(_._2 == employee) shouldBe true
    Map[Employee, CompanyRole](employee -> employee).exists(_._2 == employee) shouldBe true

    Map[Employee, Employee](employee -> employee).exists((input: (Employee, Employee)) => input._2 == employee) shouldBe true
    Map[Employee, Employee](employee -> employee).exists((input: (Employee, CompanyRole)) => input._2 == employee) shouldBe true
    Map[CompanyRole, CompanyRole](employee -> employee).exists((input: (CompanyRole, CompanyRole)) => input._2 == employee) shouldBe true
    "Map[CompanyRole, CompanyRole](employee -> employee).exists((input: (CompanyRole, Employee)) => input._2 == employee)" shouldNot typeCheck

    Map[Employee, Employee](employee -> employee).exists(Set[(Employee, Employee)](employee -> employee)) shouldBe true
    Map[Employee, Employee](employee -> employee).exists(Set[(Employee, CompanyRole)](employee -> employee)) shouldBe true
    Map[CompanyRole, CompanyRole](employee -> employee).exists(Set[(CompanyRole, CompanyRole)](employee -> employee)) shouldBe true
    Map[CompanyRole, CompanyRole](employee -> employee).exists(Set[(CompanyRole, Employee)](employee -> employee)) shouldBe true
    Map[CompanyRole, CompanyRole](employee -> employee).exists(Set(employee -> employee)) shouldBe true
  }

  test("forall on an empty Map should yield false") {
    Map.empty[Nothing, Nothing].forall(_ => false) shouldBe true
    Map.empty[Nothing, Nothing].notForall(_ => false) shouldBe false
  }

  test("forall on a non empty Map should yield true") {
    val element = randomString

    Map(element -> randomString).forall(_._1.size == element.size) shouldBe true
    Map(element -> randomString).forall(_._1.size != element.size) shouldBe false

    Map(element -> randomString).notForall(_._1.size == element.size) shouldBe false
    Map(element -> randomString).notForall(_._1.size != element.size) shouldBe true
  }

  test("Map() should not compile") {
    "Map()" shouldNot compile
  }

  test("Calling the varargs apply method on the Map companion object should yield a Map with all the arguments as elements") {
    val a = randomString
    val b = randomString
    val c = randomString

    Map(
      a -> b,
      b -> c,
      c -> a
    ) shouldBe Map.empty.add(a -> b).add(b -> c).add(c -> a)
  }

  test("isSubsetOf on an empty Map should yield true") {
    Map.empty.isSubsetOf(Map.empty) shouldBe true
    Map.empty.isSubsetOf(Map(randomString -> randomString)) shouldBe true
  }

  test("isSubsetOf on itself should yield true") {
    val map = Map(randomString -> randomString)

    map.isSubsetOf(map) shouldBe true
  }

  test("isSubsetOf on a non empty Map should yield false") {
    val a = randomString
    val b = randomString
    val c = randomString

    val left = Map(a -> b, b -> c)
    val right = left.add(c -> a)

    left.isSubsetOf(right) shouldBe true
    right.isSubsetOf(left) shouldBe false

    left.add(b -> a).isSubsetOf(right) shouldBe false
  }

  test("isSupersetOf on an empty Map should yield true") {
    Map.empty.isSupersetOf(Map.empty) shouldBe true
    Map(randomString -> randomString).isSupersetOf(Map.empty) shouldBe true
  }

  test("isSupersetOf on itself should yield true") {
    val map = Map(randomString -> randomString)

    map.isSupersetOf(map) shouldBe true
  }

  test("isSupersetOf on a non empty Map should yield false") {
    val a = randomString
    val b = randomString
    val c = randomString

    val left = Map(a -> b, b -> c)
    val right = left.add(c -> a)

    left.isSupersetOf(right) shouldBe false
    right.isSupersetOf(left) shouldBe true

    right.add(b -> a).isSupersetOf(left) shouldBe false
  }

  test("equals should be reflexive") {
    def reflexive(x: Any): Unit = {
      x shouldBe x
      x.hashCode shouldBe x.hashCode
    }

    reflexive(Map.empty)
    reflexive(Map(1 -> 1))
    reflexive(Map(1 -> 2, 2 -> 1))
    reflexive(Map(2 -> 1, 1 -> 2))
  }

  test("equals should be symmetric") {
    def symmetric(x: Any, y: Any): Unit = {
      x shouldBe y
      y shouldBe x

      x.hashCode shouldBe y.hashCode
      y.hashCode shouldBe x.hashCode
    }

    symmetric(Map.empty, Map.empty)

    symmetric(Map(1 -> 1), Map(1 -> 1))

    val upTo2 = Map(1 -> 2, 2 -> 1)
    symmetric(upTo2, upTo2)
    symmetric(upTo2, Map(2 -> 1, 1 -> 2))

    val upTo3 = Map(1 -> 2, 2 -> 3, 3 -> 1)
    symmetric(upTo3, upTo3)
    symmetric(upTo3, Map(1 -> 2, 3 -> 1, 2 -> 3))
    symmetric(upTo3, Map(2 -> 3, 1 -> 2, 3 -> 1))
    symmetric(upTo3, Map(2 -> 3, 3 -> 1, 1 -> 2))
    symmetric(upTo3, Map(3 -> 1, 1 -> 2, 2 -> 3))
    symmetric(upTo3, Map(3 -> 1, 2 -> 3, 1 -> 2))
  }

  test("equals should be transitive") {
    def transitive(x: Any, y: Any, z: Any): Unit = {
      x shouldBe y
      y shouldBe z
      x shouldBe z

      x.hashCode shouldBe y.hashCode
      y.hashCode shouldBe z.hashCode
      x.hashCode shouldBe z.hashCode
    }

    transitive(Map.empty, Map.empty, Map.empty)
    transitive(
      Map(1 -> 2, 2 -> 3, 3 -> 1),
      Map(3 -> 1, 2 -> 3, 1 -> 2),
      Map(2 -> 3, 1 -> 2, 3 -> 1)
    )
  }

  test("these should not be equal") {
    Map(1 -> 1) should not be Map(1 -> 2)
    Map(1 -> 2) should not be Map(1 -> 1)

    Map(1 -> 1) should not be 1
    1 should not be Map(1 -> 1)

    Map(1 -> 1) == 1 shouldBe false
    // 1 == Map(1 -> 1) shouldBe false

    Map(1 -> "1") should not be Map("1" -> "1")
    Map(1 -> "1") should not be Map("1" -> 1)
    Map(1 -> "1") should not be Map(1 -> 1)

    Map(1 -> "1") == Map("1" -> "1") shouldBe false
    Map(1 -> "1") == Map("1" -> 1) shouldBe false
    Map(1 -> "1") == Map(1 -> 1) shouldBe false

    Map(1 -> "I") should not be Map(1 -> "I", 2 -> "II")
    Map(1 -> "I", 2 -> "II") should not be Map(1 -> "I")
  }

  test("hashCode on an empty Map should not be random") {
    Map.empty.hashCode shouldBe Map.empty.hashCode

    val key = randomString
    val value = randomString

    Map(key -> value).hashCode shouldBe Map(key -> value).hashCode
  }

  test("hashCode on an empty Map should not be 0") {
    Map.empty.hashCode should not be 0
  }

  test("hashCode on a non empty Map should be the sum of all the hashCodes and the hashCode of the empty Map") {
    val first = randomString
    val second = randomString

    val expected = Map.empty.hashCode + (first -> second).hashCode + (second -> first).hashCode

    Map(
      first -> second,
      second -> first
    ).hashCode shouldBe expected
  }

  test("toString on an empty Map should yield Map()") {
    Map.empty.toString shouldBe "Map()"
  }

  test("toString on a Map with one element should yield Map(key -> value)") {
    val key = randomString
    val value = randomString

    Map(key -> value).toString shouldBe s"Map($key -> $value)"
  }

  test("toString on a Map with two elements should contain 2 parens, both elements, 2 arrows and one comma") {
    val first = randomString
    val second = randomString

    first should not be second

    val map = Map(first -> second, second -> first)

    val actual = map.toString

    actual.count(_ == '(') shouldBe 1

    actual should include(first)

    actual.count(_ == ',') shouldBe 1

    actual should include(second)

    actual.count(_ == ')') shouldBe 1

    // arrows
    actual.count(_ == '-') shouldBe 2
    actual.count(_ == '>') shouldBe 2
  }

  test("toString on a Map with three elements should contain 2 parens, three elements, 3 arrows and two commas") {
    val first = randomString
    val second = randomString
    val third = randomString

    first should not be second
    second should not be third

    val map =
      Map(
        first -> second,
        second -> third,
        third -> first
      )

    val actual = map.toString

    actual.count(_ == '(') shouldBe 1

    actual should include(first)

    actual.count(_ == ',') shouldBe 2

    actual should include(second)

    actual should include(third)

    actual.count(_ == ')') shouldBe 1

    // arrows
    actual.count(_ == '-') shouldBe 3
    actual.count(_ == '>') shouldBe 3
  }

  test("toString should not produce any commas with leading spaces") {
    Map(1 -> 0, 0 -> 1).toString should not include (" ,")
  }

  test("map on an empty Map should not apply the function") {
    noException should be thrownBy Map.empty[Nothing, Nothing].map(_ => sys.error("should not be thrown"))
  }

  test("map should produce a Map") {
    val input =
      Map(
        "hello" -> "world",
        "world" -> "hello"
      )

    val expected =
      Map(
        "dlrow" -> "olleh",
        "olleh" -> "dlrow"
      )

    input.map {
      case (key, value) => key.reverse -> value.reverse
    } shouldBe expected
  }

  test("map should be able to produce a Map of sth else other than String") {
    Map("hello" -> "planet").map {
      case (key, value) => key.size -> value.size
    } shouldBe Map(5 -> 6)

    Map(
      "hello" -> "planet",
      "world" -> "whatever"
    ).map {
        case (key, value) => key.size -> value.size
      } shouldBe Map(5 -> 8) // note that the resulting map has only 1 key instead of 2
  }

  test("flatMap should be able to produce a chessboard") {
    val alphabet = Map('a' -> 'a'.hashCode, 'b' -> 'b'.hashCode)
    val even = Map('a'.hashCode -> false, 'b'.hashCode -> true)

    val result: Map[Char, Boolean] =
      alphabet.flatMap {
        case (aKey, aValue) =>
          even.map {
            case (eKey, eValue) =>
              aKey -> eValue
          }
      }

    result shouldBe Map(
      'a' -> true,
      'b' -> true
    )
  }
}
