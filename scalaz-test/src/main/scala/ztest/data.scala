package scalaz.test

import scala._, scala.Predef._

import scalaz.Scalaz._, scalaz.core.EqClass

trait Harness[F[_], G[_]] {
  def section[A](annotation: String)(test: G[A]): G[A]
  def shared[A](fa: F[A]): G[A]
  def test(assertion: F[TestResult]): G[Unit]
  final def namedTest(annotation: String)(errs: F[TestResult]): G[Unit] =
    section(annotation)(test(errs))
}

sealed trait TestResult
final class Thrown(val thrown: Throwable) extends TestResult
object Thrown {
  def apply(thrown: Throwable): TestResult = new Thrown(thrown)
}
object Success extends TestResult {
  def apply(): TestResult = this
}
object Failure extends TestResult {
  def apply(): TestResult = this
}
object TestResult {
  implicit val equalTestResult: Eq[TestResult] = instanceOf(new EqClass[TestResult] {
    def equal(first: TestResult, second: TestResult) = first == second
  })
}
