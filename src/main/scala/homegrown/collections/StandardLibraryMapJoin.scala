import scala.collection.Map

object StandardLibraryMapJoin {
  final implicit class DSL[Key, ThisValue](private val self: Map[Key, ThisValue]) {
    def join[ThatValue](that: Map[Key, ThatValue]): Join[Key, ThisValue, ThatValue] =
      new Join(self, that)

    class Join[Key, ThisValue, ThatValue](
        self: Map[Key, ThisValue],
        that: Map[Key, ThatValue]
    ) {
      def inner: Map[Key, (ThisValue, ThatValue)] =
        inner(T2)

      def inner[TargetValue](factory: (ThisValue, ThatValue) => TargetValue): Map[Key, TargetValue] =
        self.foldLeft[Map[Key, TargetValue]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that.get(thisKey)
              .map { thatValue =>
                acc.+(thisKey -> factory(thisValue, thatValue))
              }
              .getOrElse {
                acc
              }
        }

      def leftOuter: Map[Key, (ThisValue, Option[ThatValue])] =
        leftOuter(T2)

      def leftOuter[TargetValue](factory: (ThisValue, Option[ThatValue]) => TargetValue): Map[Key, TargetValue] =
        self.foldLeft[Map[Key, TargetValue]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that.get(thisKey)
              .map { thatValue =>
                acc.+(thisKey -> factory(thisValue, Some(thatValue)))
              }
              .getOrElse {
                acc.+(thisKey -> factory(thisValue, None))
              }
        }

      def leftOnly: Map[Key, (ThisValue, Option[ThatValue])] =
        leftOnly(T2)

      def leftOnly[TargetValue](factory: (ThisValue, Option[ThatValue]) => TargetValue): Map[Key, TargetValue] =
        self.foldLeft[Map[Key, TargetValue]](Map.empty) {
          case (acc, (thisKey, thisValue)) =>
            that.get(thisKey)
              .map { thatValue =>
                acc
              }
              .getOrElse {
                acc.+(thisKey -> factory(thisValue, None))
              }
        }

      def rightOuter: Map[Key, (Option[ThisValue], ThatValue)] =
        rightOuter(T2)

      def rightOuter[TargetValue](factory: (Option[ThisValue], ThatValue) => TargetValue): Map[Key, TargetValue] =
        that.foldLeft[Map[Key, TargetValue]](Map.empty) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey)
              .map { thisValue =>
                acc.+(thatKey -> factory(Some(thisValue), thatValue))
              }
              .getOrElse {
                acc.+(thatKey -> factory(None, thatValue))
              }
        }

      def rightOnly: Map[Key, (Option[ThisValue], ThatValue)] =
        rightOnly(T2)

      def rightOnly[TargetValue](factory: (Option[ThisValue], ThatValue) => TargetValue): Map[Key, TargetValue] =
        that.foldLeft[Map[Key, TargetValue]](Map.empty) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey)
              .map { thisValue =>
                acc
              }
              .getOrElse {
                acc.+(thatKey -> factory(None, thatValue))
              }
        }

      def fullOuter: Map[Key, (Option[ThisValue], Option[ThatValue])] =
        fullOuter(T2)

      def fullOuter[TargetValue](factory: (Option[ThisValue], Option[ThatValue]) => TargetValue): Map[Key, TargetValue] = {
        val left =
          self.foldLeft[Map[Key, TargetValue]](Map.empty) {
            case (acc, (thisKey, thisValue)) =>
              that.get(thisKey)
                .map { thatValue =>
                  acc.+(thisKey -> factory(Some(thisValue), Some(thatValue)))
                }
                .getOrElse {
                  acc.+(thisKey -> factory(Some(thisValue), None))
                }
          }

        that.foldLeft[Map[Key, TargetValue]](left) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey)
              .map { thisValue =>
                acc
              }
              .getOrElse {
                acc.+(thatKey -> factory(None, Some(thatValue)))
              }
        }
      }

      def outer: Map[Key, (Option[ThisValue], Option[ThatValue])] =
        outer(T2)

      def outer[TargetValue](factory: (Option[ThisValue], Option[ThatValue]) => TargetValue): Map[Key, TargetValue] = {
        val left =
          self.foldLeft[Map[Key, TargetValue]](Map.empty) {
            case (acc, (thisKey, thisValue)) =>
              that.get(thisKey)
                .map { thatValue =>
                  acc
                }
                .getOrElse {
                  acc.+(thisKey -> factory(Some(thisValue), None))
                }
          }

        that.foldLeft[Map[Key, TargetValue]](left) {
          case (acc, (thatKey, thatValue)) =>
            self.get(thatKey)
              .map { thisValue =>
                acc
              }
              .getOrElse {
                acc.+(thatKey -> factory(None, Some(thatValue)))
              }
        }
      }
    }
  }

  private def T2[First, Second]: (First, Second) => Tuple2[First, Second] =
    Tuple2.apply
}

object StandardLibraryMapJoinApp extends App {
  import StandardLibraryMapJoin._

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

  println("─" * 50)

  println(people.join(cars).fullOuter.mkString("\n"))

  println("─" * 50)
}
