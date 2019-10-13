package homegrown.collections

final class Map[Key, +Value] private (
    val keys: Set[Key],
    valueOf: Key => Option[Value],
    default: Option[Key => Value])
    extends Function1[Key, Option[Value]]
    with FoldableFactory2[Key, Value, Map] {
  final override protected def factory: Factory2[Map] =
    Map

  final override def apply(key: Key): Option[Value] =
    valueOf(key) orElse default.map(_ apply key)

  final lazy val values: Set[Value] =
    keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: Key): Value =
    valueOf(key).get

  final override def foldLeft[Result](
      seed: Result
    )(
      function: (Result, => (Key, Value)) => Result
    ): Result =
    keys.foldLeft(seed) { (acc, currentKey) =>
      function(acc, currentKey -> unsafeValueOf(currentKey))
    }

  final override def foldRight[Result](
      seed: => Result
    )(
      function: (=> (Key, Value), => Result) => Result
    ): Result =
    keys.foldRight(seed) { (currentKey, acc) =>
      function(currentKey -> unsafeValueOf(currentKey), acc)
    }

  final def add[SuperValue >: Value](
      input: (Key, SuperValue)
    ): Map[Key, SuperValue] = {
    val (key, value) = input

    copy(
      keys = keys.add(key),
      valueOf = {
        case `key` => Some(value)
        case k     => valueOf(k)
      }
    )
  }

  final def remove(key: Key): Map[Key, Value] =
    copy(
      keys = keys.remove(key),
      valueOf = {
        case `key` => None
        case k     => valueOf(k)
      }
    )

  final def isSubsetOf[SuperValue >: Value](
      that: Map[Key, SuperValue]
    ): Boolean =
    forall {
      case (key, value) => that(key) == Some(value)
    }

  final def isSupersetOf[SuperValue >: Value](
      that: Map[Key, SuperValue]
    ): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean =
    other match {
      case that: Map[Key, Value] =>
        this.isSubsetOf(that) && that.isSubsetOf(this)
      case _ => false
    }

  final override def hashCode: Int =
    foldLeft(41)(_ + _.hashCode)

  final override def toString: String =
    s"HGCMap($toStringContent)"

  private[this] def toStringContent: String = keys.tree match {
    case Tree.Empty =>
      ""

    case Tree.NonEmpty(left, key, right) => // format: OFF
      unsafeRendered(key) +
       left.map(unsafeRendered).splitByCommaSpace +
      right.map(unsafeRendered).splitByCommaSpace // format: ON
  }

  private[this] def unsafeRendered(key: Key): String =
    s"$key -> ${unsafeValueOf(key)}"

  final def isEmpty: Boolean =
    keys.isEmpty

  final def nonEmpty: Boolean =
    !isEmpty

  final def isSingleton: Boolean =
    keys.isSingleton

  final def sample: Option[(Key, Value)] =
    keys.sample.map(key => key -> unsafeValueOf(key))

  final def withDefaultValue[SuperValue >: Value](
      defaultValue: => SuperValue
    ): Map[Key, SuperValue] =
    withDefault(_ => defaultValue)

  final def withDefault[SuperValue >: Value](
      default: Key => SuperValue
    ): Map[Key, SuperValue] =
    copy(default = Some(default))

  final def getOrElseUpdated[SuperValue >: Value](
      key: Key,
      newValue: => SuperValue
    ): (SuperValue, Map[Key, SuperValue]) =
    valueOf(key)
      .map(_ -> this)
      .getOrElse {
        val value = newValue

        value -> add(key -> value)
      }

  final def mapValues[NewValue](
      function: Value => NewValue
    ): Map[Key, NewValue] =
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

  final private[this] def copy[SuperValue >: Value](
      keys: Set[Key] = keys,
      valueOf: Key => Option[SuperValue] = valueOf,
      default: Option[Key => SuperValue] = default
    ): Map[Key, SuperValue] =
    Map(keys, valueOf, default)
}

object Map extends Factory2[Map] {
  final override def empty[Key, Value]: Map[Key, Value] =
    apply(
      keys = Set.empty,
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
    final def andSomeValues[Value](
        valueOf: PartialFunction[Element, Value]
      ): Map[Element, Value] =
      andValues(valueOf.lift)

    final def andValues[Value](
        valueOf: Element => Option[Value]
      ): Map[Element, Value] =
      keys.foldLeft[Map[Element, Value]](Map.empty) { (acc, currentKey) =>
        valueOf(currentKey)
          .map(value => acc.add(currentKey -> value))
          .getOrElse(acc)
      }
  }

  object PotentiallyDangerousImplicits {
    final implicit class MapExtensions[Key, Value](val self: Map[Key, Value])
        extends AnyVal {
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
