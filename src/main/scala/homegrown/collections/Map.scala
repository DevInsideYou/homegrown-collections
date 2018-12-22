package homegrown.collections

final class Map[Key, +Value] private (
    val keys: Set[Key], // domain
    valueOf: Key => Option[Value]
) extends Function1[Key, Option[Value]]
  with FoldableFactory2[Key, Value, Map] {
  final override protected def factory: Factory2[Map] =
    Map

  final override def apply(key: Key): Option[Value] =
    valueOf(key)

  final lazy val values: Set[Value] =
    keys.map(unsafeValueOf)

  private[this] def unsafeValueOf(key: Key): Value =
    valueOf(key).get

  final override def fold[Result](seed: Result)(function: (Result, (Key, Value)) => Result): Result =
    keys.fold(seed) { (acc, currentKey) =>
      function(acc, currentKey -> unsafeValueOf(currentKey))
    }

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

  final def isSubsetOf[SuperValue >: Value](that: Map[Key, SuperValue]): Boolean =
    forall {
      case (key, value) => that(key) == Some(value)
    }

  final def isSupersetOf[SuperValue >: Value](that: Map[Key, SuperValue]): Boolean =
    that.isSubsetOf(this)

  final override def equals(other: Any): Boolean =
    other match {
      case that: Map[Key, Value] => this.isSubsetOf(that) && that.isSubsetOf(this)
      case _                     => false
    }

  final override def hashCode: Int =
    fold(41)(_ + _.hashCode)

  final override def toString: String = keys.tree match {
    case Tree.Empty =>
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
}

object Map extends Factory2[Map] {
  final override def empty[Key, Value]: Map[Key, Value] =
    apply(
      keys    = Set.empty,
      valueOf = _ => None
    )

  private def apply[Key, Value](
      keys: Set[Key],
      valueOf: Key => Option[Value]
  ): Map[Key, Value] =
    new Map(keys, valueOf)
}
