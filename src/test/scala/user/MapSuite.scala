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
}
