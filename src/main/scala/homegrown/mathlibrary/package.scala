package homegrown

import org.scalacheck.Prop._

package object mathlibrary {
  type ClosedBinaryOperation[A] = (A, A) => A

  private[mathlibrary] type Law = org.scalacheck.Prop

  type Arbitrary[A] = org.scalacheck.Arbitrary[A]

  type Gen[A] = org.scalacheck.Gen[A]

  @inline def Arbitrary: org.scalacheck.Arbitrary.type =
    org.scalacheck.Arbitrary

  @inline def Gen: org.scalacheck.Gen.type =
    org.scalacheck.Gen

  implicit final class InfixNotation[A](private val a1: A) extends AnyVal {
    @inline def combine(a2: A)(implicit magma: Magma[A]): A =
      magma.operation(a1, a2)
  }

  def closure[A: Arbitrary: Magma](
      operation: ClosedBinaryOperation[A]
  ): Law = forAll { (a1: A, a2: A) =>
    val a3: A = a1 combine a2

    true
  }

  def associativity[A: Arbitrary: Magma](
      operation: ClosedBinaryOperation[A]
  ): Law = forAll { (a1: A, a2: A, a3: A) => // format: OFF
    val chaining = (a1 combine  a2) combine a3  // a1.combine(a2)
                                                //   .combine(a3)

    val  nesting =  a1 combine (a2 combine a3)  // a1.combine(
                                                //   a2.combine(a3)
                                                // )

    chaining == nesting
  } // format: ON

  def identity[A: Arbitrary: Magma](
      operation: ClosedBinaryOperation[A],
      uniqueIdentityElement: A
  ): Law = forAll { a: A => // format: OFF
    Predef.identity(a)               == a &&
    a.combine(uniqueIdentityElement) == a && //  left identity
    uniqueIdentityElement.combine(a) == a    // right identity
  } // format: ON

  def invertibility[A: Arbitrary: Magma](
      operation: ClosedBinaryOperation[A],
      uniqueIdentityElement: A,
      uniqueInverseElement: A => A
  ): Law = forAll { a: A => // format: OFF
    a.combine(uniqueInverseElement(a)) == uniqueIdentityElement &&
    uniqueInverseElement(a).combine(a) == uniqueIdentityElement
  } // format: ON

  def commutativity[A: Arbitrary: Magma](
      operation: ClosedBinaryOperation[A]
  ): Law = forAll { (a1: A, a2: A) =>
    a1.combine(a2) == a2.combine(a1)
  }

  def distributivity[A: Arbitrary](
      plus: ClosedBinaryOperation[A],
      times: ClosedBinaryOperation[A]
  ): Law = forAll { (a1: A, a2: A, a3: A) => // format: OFF
    // a1 * (a2 + a3) == (a1 * a2) + (a1 * a3) // left distributivity
    // (a2 + a2) * a1 == (a2 * a1) + (a3 * a1) // right distributivity

    times(a1, plus(a2, a3)) == plus(times(a1, a2), times(a1, a3)) &&
    times(plus(a2, a3), a1) == plus(times(a2, a1), times(a3, a1))
  } // format: ON

  implicit lazy val IntAddition: Ring[Int] =
    new Ring[Int] {
      final override protected lazy val arbitrary: Arbitrary[Int] =
        implicitly[Arbitrary[Int]]

      final override lazy val operation: ClosedBinaryOperation[Int] =
        _ + _

      final override lazy val uniqueIdentityElement: Int =
        0

      final override lazy val uniqueInverseElement: Int => Int =
        -_

      final override lazy val that: Monoid[Int] =
        IntMultiplication
    }

  implicit lazy val IntMultiplication: Monoid[Int] =
    new Monoid[Int] {
      final override protected lazy val arbitrary: Arbitrary[Int] =
        implicitly[Arbitrary[Int]]

      final override lazy val operation: ClosedBinaryOperation[Int] =
        _ * _

      final override lazy val uniqueIdentityElement: Int =
        1
    }

  implicit val StringConcatenation: Monoid[String] =
    new Monoid[String] {
      final override protected lazy val arbitrary: Arbitrary[String] =
        implicitly[Arbitrary[String]]

      final override lazy val operation: ClosedBinaryOperation[String] =
        _ + _

      final override lazy val uniqueIdentityElement: String =
        ""
    }

  trait BooleanAdditionMonoid extends Monoid[Boolean] {
    final override protected lazy val arbitrary: Arbitrary[Boolean] =
      implicitly[Arbitrary[Boolean]]

    final override lazy val operation: ClosedBinaryOperation[Boolean] =
      _ || _

    final override lazy val uniqueIdentityElement: Boolean =
      false
  }

  object BooleanAdditionMonoid extends BooleanAdditionMonoid

  implicit object BooleanAddition extends Rig[Boolean] with BooleanAdditionMonoid {
    final override lazy val that: Monoid[Boolean] =
      BooleanMultiplicationMonoid
  }

  trait BooleanMultiplicationMonoid extends Monoid[Boolean] {
    final override protected lazy val arbitrary: Arbitrary[Boolean] =
      implicitly[Arbitrary[Boolean]]

    final override lazy val operation: ClosedBinaryOperation[Boolean] =
      _ && _

    final override lazy val uniqueIdentityElement: Boolean =
      true
  }

  object BooleanMultiplicationMonoid extends BooleanMultiplicationMonoid

  implicit object BooleanMultiplication extends Rig[Boolean] with BooleanMultiplicationMonoid {
    final override lazy val that: Monoid[Boolean] =
      BooleanAdditionMonoid
  }
}
