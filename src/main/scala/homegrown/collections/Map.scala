package homegrown.collections

final class Map[Key, +Value] private (
    val keys: Set[Key], // domain
    valueOf: Key => Option[Value]
) extends Function1[Key, Option[Value]] {
  final override def apply(key: Key): Option[Value] =
    valueOf(key)

  final lazy val values: Set[Value] =
    keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: Key): Value =
    valueOf(key).get

  final def add[SuperValue >: Value](input: (Key, SuperValue)): Map[Key, SuperValue] = {
    val (key, value) = input

    Map(
      keys    = keys.add(key),
      valueOf = {
        case `key` => Some(value)
        case k     => valueOf(k)
      }
    )
  }

  final def remove(key: Key): Map[Key, Value] =
    Map(
      keys    = keys.remove(key),
      valueOf = {
        case `key` => None
        case k     => valueOf(k)
      }
    )

  final def isEmpty: Boolean =
    keys.isEmpty

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean =
    keys.isSingleton

  final def sample: Option[(Key, Value)] =
    keys.sample.map(key => key -> unsafeValueOf(key))
}

object Map {
  final def empty[Key, Value]: Map[Key, Value] =
    apply(
      keys    = Set.empty,
      valueOf = _ => None
    )

  private def apply[Key, Value](
      keys: Set[Key],
      valueOf: Key => Option[Value]
  ): Map[Key, Value] =
    new Map(keys, valueOf)

  final def apply[Key, Value](element: (Key, Value), otherElements: (Key, Value)*): Map[Key, Value] =
    otherElements.foldLeft[Map[Key, Value]](empty.add(element))(_ add _)
}
