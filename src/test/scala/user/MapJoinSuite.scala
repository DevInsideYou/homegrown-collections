package user

import homegrown.collections._

import org.scalatest._

class MapJoinSuite extends FunSuite with Matchers {
  import MapJoinSuite._
  import MapJoin._

  test("inner join") {
    people.join(cars).inner shouldBe Map(
      1 -> ("alfa", "audi"),
      3 -> ("charlie", "cadillac")
    )

    people.join(cars).inner(PersonWithCarBrand) shouldBe Map(
      1 -> PersonWithCarBrand("alfa", "audi"),
      3 -> PersonWithCarBrand("charlie", "cadillac")
    )

    val colors =
      Map(
        // 1 is missing
        // 2 is missing
        3 -> "blue"
      // 4 is missing
      )

    people
      .join(cars).inner
      .join(colors).inner shouldBe Map(
        3 -> (("charlie", "cadillac") -> "blue")
      )
  }

  test("left outer join") {
    people.join(cars).leftOuter shouldBe Map(
      1 -> ("alfa", Some("audi")),
      3 -> ("charlie", Some("cadillac")),
      4 -> ("delta", None)
    )
  }

  test("left only join") {
    people.join(cars).leftOnly shouldBe Map(
      4 -> ("delta", None)
    )
  }

  test("right outer join") {
    people.join(cars).rightOuter shouldBe Map(
      1 -> (Some("alfa"), "audi"),
      2 -> (None, "bmw"),
      3 -> (Some("charlie"), "cadillac")
    )
  }

  test("right only join") {
    people.join(cars).rightOnly shouldBe Map(
      2 -> (None, "bmw")
    )
  }

  test("full outer join") {
    people.join(cars).fullOuter shouldBe Map(
      1 -> (Some("alfa"), Some("audi")),
      2 -> (None, Some("bmw")),
      3 -> (Some("charlie"), Some("cadillac")),
      4 -> (Some("delta"), None)
    )
  }

  test("outer (not inner) join") {
    people.join(cars).outer shouldBe Map(
      2 -> (None, Some("bmw")),
      4 -> (Some("delta"), None)
    )
  }
}

object MapJoinSuite {
  /*

   People             Cars
  ┌────┬───────────┐ ┌──────┬────────────┬────────┐
  │ ID │ Name      │ │ ID   │ Brand      │ Owner  │
  ├────┼───────────┤ ├──────┼────────────┼────────┤
  │ 1  │ "alfa"    │ │ 9    │ "audi"     │ 1      │
  │    │           │ │ 99   │ "bmw"      │ 2      │
  │ 3  │ "charlie" │ │ 999  │ "cadillac" │ 3      │
  │ 4  │ "delta"   │ │      │            │        │
  └────┴───────────┘ └──────┴────────────┴────────┘

   Join
  ┌───────────┬─────────────┬────────────┐
  │ Person.ID │ Person.Name │ Car.Brand  │
  ├───────────┼─────────────┼────────────┤
  │     1     │ "alfa"      │ "audi"     │
  │     2     │             │ "bmw"      │
  │     3     │ "charlie"   │ "cadillac" │
  │     4     │ "delta"     │            │
  └───────────┴─────────────┴────────────┘
  */

  type PersonId = Int
  type PersonName = String
  type CarBrand = String

  val people: Map[PersonId, PersonName] =
    Map(
      1 -> "alfa",
      // 2 is missing
      3 -> "charlie",
      4 -> "delta"
    )

  val cars: Map[PersonId, CarBrand] =
    Map(
      1 -> "audi",
      2 -> "bmw",
      3 -> "cadillac"
    // 4 is missing
    )

  final case class PersonWithCarBrand(name: String, carBrand: String)
}
