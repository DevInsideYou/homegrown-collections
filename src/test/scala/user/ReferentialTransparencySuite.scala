package user

import homegrown.collections._

class ReferentialTransparencySuite extends TestSuite {
  import Map.PotentiallyDangerousImplicits._

  ignore("hello world") {
    val someValue = "HULK SMASH!!!"

    val setBeforeDB = Set(1, 2, 3)
    val setAfterDB = Set(3, 2, 1)

    val setBeforeDBMapped =
      setBeforeDB.map(_ => someValue)

    val setAfterDBMapped =
      setAfterDB.map(_ => someValue)

    setBeforeDBMapped shouldBe Set(someValue)
    setAfterDBMapped shouldBe Set(someValue)

    if (setBeforeDB == setAfterDBMapped)
      setBeforeDBMapped shouldBe setAfterDBMapped

    val values: PartialFunction[Int, String] = {
      case 1 => "I"
      case 2 => "II"
      case 3 => "III"
    }

    val mapBeforeDB =
      Map.withKeys(setBeforeDB).andSomeValues(values)

    val mapAfterDB =
      Map.withKeys(setAfterDB).andSomeValues(values)

    val mapBeforeDBMapped =
      mapBeforeDB.mapKeys(_ => someValue)

    val mapAfterDBMapped =
      mapAfterDB.mapKeys(_ => someValue)

    if (mapBeforeDB == mapAfterDB)
      mapBeforeDBMapped shouldBe mapAfterDBMapped
  }
}
