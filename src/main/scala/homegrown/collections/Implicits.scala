object Impicits extends App {
  final case class Apple(weightInGrams: Int, color: String)
  final case class Orange(weightInGrams: Int)

  def function(orange: Orange): Unit = {
    println("─" * 50)

    println(orange)

    println("─" * 50)
  }

  implicit def AppleCanBeUsedAsOrange(apple: Apple): Orange =
    Orange(apple.weightInGrams)

  function(
    // AppleCanBeUsedAsOrange(
    Apple(
      weightInGrams = 300,
      color         = "red"
    )
  // )
  )

  // implicit def AppleWrapper(apple: Apple): AppleWrapper =
  //   new AppleWrapper(apple)

  implicit final class AppleWrapper(val apple: Apple) extends AnyVal {
    def toOrange: Orange =
      Orange(apple.weightInGrams)
  }

  function(
    AppleWrapper(
      Apple(
        weightInGrams = 300,
        color         = "red"
      )
    ).toOrange
  )
}
