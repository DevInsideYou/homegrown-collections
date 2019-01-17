final case class Flight(source: String, destination: String) {
  final override def toString: String =
    s"Flight from $source to $destination"
}

object Flight {
  def from(source: String): Source =
    new Source(source)

  final class Source private[Flight] (source: String) {
    def to(destination: String): Flight =
      Flight(source, destination)
  }
}

object Airport extends App {
  println("─" * 50)

  val regularFlight = Flight("Stuttgart", "Berlin")

  println(regularFlight)

  val fancyFlight = Flight from "Stuttgart" to "Berlin"

  println(fancyFlight)

  println(regularFlight == fancyFlight)

  println("─" * 50)
}
