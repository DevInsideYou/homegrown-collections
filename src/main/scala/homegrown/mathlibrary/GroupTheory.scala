package homegrown.mathlibrary

trait Summoner[B[_]] {
  def apply[A: B]: B[A] =
    implicitly[B[A]]
}

trait Magma[A] extends AlgebraicStructure[A] {
  def operation: ClosedBinaryOperation[A]

  protected implicit def arbitrary: Arbitrary[A]

  final protected implicit def self: Magma[A] =
    this

  override def laws: Set[Law] =
    Set(closure(operation))
}

object Magma extends Summoner[Magma]

trait Semigroup[A] extends Magma[A] {
  override def laws: Set[Law] =
    super.laws + associativity(operation)
}

object Semigroup extends Summoner[Semigroup]

trait Monoid[A] extends Semigroup[A] {
  def uniqueIdentityElement: A

  override def laws: Set[Law] =
    super.laws + identityLaw(
      operation,
      uniqueIdentityElement
    )
}

object Monoid extends Summoner[Monoid]

trait Group[A] extends Monoid[A] {
  def uniqueInverseElement: A => A

  override def laws: Set[Law] =
    super.laws + invertibility(
      operation,
      uniqueIdentityElement,
      uniqueInverseElement
    )
}

object Group extends Summoner[Group]

trait Abelian[A] extends Magma[A] {
  override def laws: Set[Law] =
    super.laws + commutativity(operation)
}

trait AbelianGroup[A] extends Group[A] with Abelian[A]
object AbelianGroup extends Summoner[AbelianGroup]

trait Rng[A] extends AbelianGroup[A] {
  def that: Semigroup[A]

  override def laws: Set[Law] =
    super.laws ++ that.laws + distributivity(
      this.operation,
      that.operation
    )
}

object Rng extends Summoner[Rng]

trait Ring[A] extends Rng[A] {
  def that: Monoid[A]
}

object Ring extends Summoner[Ring]

trait AbelianMonoid[A] extends Monoid[A] with Abelian[A]
object AbelianMonoid extends Summoner[AbelianMonoid]

trait Rig[A] extends AbelianMonoid[A] {
  def that: Monoid[A]

  override def laws: Set[Law] =
    super.laws ++ that.laws + distributivity(
      this.operation,
      that.operation
    )
}

object Rig extends Summoner[Rig]

trait AbelianSemigroup[A] extends Semigroup[A] with Abelian[A]
object AbelianSemigroup extends Summoner[AbelianSemigroup]

trait Semiring[A] extends AbelianSemigroup[A] {
  def that: Monoid[A]

  override def laws: Set[Law] =
    super.laws ++ that.laws + distributivity(
      this.operation,
      that.operation
    )
}
