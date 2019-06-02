package user

import org.scalatest._
import org.scalatestplus.scalacheck._

trait TestSuite extends FunSuite with Matchers with ScalaCheckPropertyChecks
