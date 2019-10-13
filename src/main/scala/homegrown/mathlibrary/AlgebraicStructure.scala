package homegrown.mathlibrary

import org.scalacheck._
import org.scalacheck.Test._

trait AlgebraicStructure[A] {
  AlgebraicStructure.ensureLawsHold(laws)

  def laws: Set[Law]
}

object AlgebraicStructure {
  private def ensureLawsHold(laws: Set[Law]): Unit = {
    Test.check(parameters, laws.reduce(_ && _))
  }

  private val parameters: Parameters =
    Parameters.default
      .withTestCallback {
        new TestCallback {
          final override def onTestResult(
              name: String,
              result: Result
            ): Unit = {
            if (!result.passed)
              sys.error(util.Pretty.pretty(result))
          }
        }
      }
}
