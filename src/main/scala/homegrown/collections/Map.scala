package homegrown.collections

final class Map[Key, Value] private (
    val keys: Set[Key],
    valueOf: Key => Option[Value],
    default: Option[Key => Value]
) extends Function1[Key, Option[Value]]
  with FoldableFactory2[Key, Value, Map] {
  final override protected def factory: Factory2[Map] =
    Map

  final override def apply(key: Key): Option[Value] =
    valueOf(key) orElse default.map(_ apply key)

  final lazy val values: Set[Value] =
    keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: Key): Value =
    valueOf(key).get

  final override def fold[Result](seed: Result)(function: (Result, (Key, Value)) => Result): Result =
    keys.fold(seed) { (acc, currentKey) =>
      function(acc, currentKey -> unsafeValueOf(currentKey))
    }

  final def add(input: (Key, Value)): Map[Key, Value] = {
    val (key, value) = input

    copy(
      keys    = keys.add(key),
      valueOf = {
        case `key` => Some(value)
        case k     => valueOf(k)
      }
    )
  }

  final def remove(key: Key): Map[Key, Value] =
    copy(
      keys    = keys.remove(key),
      valueOf = {
        case `key` => None
        case k     => valueOf(k)
      }
    )

  final def isSubsetOf(that: Map[Key, Value]): Boolean =
    forall {
      case (key, value) => that(key) == Some(value)
    }

  final def isSupersetOf(that: Map[Key, Value]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean =
    other match {
      case that: Map[Key, Value] => this.isSubsetOf(that) && that.isSubsetOf(this)
      case _                     => false
    }

  final override def hashCode: Int =
    fold(41)(_ + _.hashCode)

  final override def toString: String = keys.tree match {
    case Tree.Empty() =>
      "Map()"

    case Tree.NonEmpty(left, key, right) =>
      "Map(" + unsafeRendered(key) + splitByCommaSpace(left) + splitByCommaSpace(right) + ")"
  }

  private[this] def unsafeRendered(key: Key): String =
    s"$key -> ${unsafeValueOf(key)}"

  private[this] def splitByCommaSpace(input: Tree[Key]) =
    input.fold("") { (acc, currentKey) =>
      s"$acc, ${unsafeRendered(currentKey)}"
    }

  final def isEmpty: Boolean =
    keys.isEmpty

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean =
    keys.isSingleton

  final def sample: Option[(Key, Value)] =
    keys.sample.map(key => key -> unsafeValueOf(key))

  final def withDefaultValue(defaultValue: => Value): Map[Key, Value] =
    withDefault(_ => defaultValue)

  final def withDefault(default: Key => Value): Map[Key, Value] =
    copy(default = Some(default))

  final def getOrElseUpdated(key: Key, newValue: => Value): (Value, Map[Key, Value]) =
    valueOf(key)
      .map(_ -> this)
      .getOrElse {
        val value = newValue

        value -> add(key -> value)
      }

  final def mapValues[NewValue](function: Value => NewValue): Map[Key, NewValue] =
    map {
      case (key, value) => key -> function(value)
    }

  final def filterKeys(predicate: Key => Boolean): Map[Key, Value] =
    filter {
      case (key, _) => predicate(key)
    }

  final def filterValues(predicate: Value => Boolean): Map[Key, Value] =
    filter {
      case (_, value) => predicate(value)
    }

  private[this] final def copy(
      keys: Set[Key] = keys,
      valueOf: Key => Option[Value] = valueOf,
      default: Option[Key => Value] = default
  ): Map[Key, Value] =
    Map(keys, valueOf, default)
}

object Map extends Factory2[Map] {
  final override def empty[Key, Value]: Map[Key, Value] =
    apply(
      keys    = Set.empty,
      valueOf = _ => None,
      default = None
    )

  private def apply[Key, Value](
      keys: Set[Key],
      valueOf: Key => Option[Value],
      default: Option[Key => Value]
  ): Map[Key, Value] =
    new Map(keys, valueOf, default)

  def withKeys[Element](keys: Set[Element]): Source[Element] =
    new Source(keys)

  final class Source[Element](val keys: Set[Element]) extends AnyVal {
    final def andSomeValues[Value](valueOf: PartialFunction[Element, Value]): Map[Element, Value] =
      andValues(valueOf.lift)

    final def andValues[Value](valueOf: Element => Option[Value]): Map[Element, Value] =
      keys.fold[Map[Element, Value]](Map.empty) { (acc, currentKey) =>
        valueOf(currentKey)
          .map(value => acc.add(currentKey -> value))
          .getOrElse(acc)
      }
  }

  object PotentiallyDangerousImplicits {
    final implicit class MapExtensions[Key, Value](val self: Map[Key, Value]) extends AnyVal {
      final def mapKeys[NewKey](function: Key => NewKey): Map[NewKey, Value] =
        self.map {
          case (key, value) => function(key) -> value
        }

      final def swapped: Map[Value, Key] =
        self.map {
          case (key, value) => value -> key
        }
    }
  }
}
