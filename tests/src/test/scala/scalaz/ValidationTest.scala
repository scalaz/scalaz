package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ValidationTest extends Spec {
  import std.AllInstances._

  checkAll("Validation", order.laws[Validation[Int, Int]])
  checkAll("FailProjection", order.laws[FailProjection[Int, Int]])

  type ValidationInt[A] = Validation[Int, A]
  type FailProjectionInt[A] = FailProjection[Int, A]

  checkAll("Validation", semigroup.laws[ValidationInt[Int]])
  checkAll("Validation", plus.laws[ValidationInt])
  checkAll("Validation", applicative.laws[ValidationInt])
  checkAll("Validation", traverse.laws[ValidationInt])
  checkAll("Validation", bifunctor.laws[Validation])

  checkAll("FailProjection", semigroup.laws[FailProjectionInt[Int]])
  checkAll("FailProjection", plus.laws[FailProjectionInt])
  checkAll("FailProjection", applicative.laws[FailProjectionInt])
  checkAll("FailProjection", traverse.laws[FailProjectionInt])

  checkAll("FailProjection", bifunctor.laws[FailProjection])

  "fpoint and point" in {

    import syntax.pointed._
    import std.AllInstances._
    import Validation._


    val vi = success[String, Int](0)

    val voi: Validation[String, Option[Int]] = vi.pointSuccess[Option, Int]
    val ovi: Option[Validation[String, Int]] = vi.point[Option]
    voi must be_===(success[String, Option[Int]](Some(0)))
    ovi must be_===(Some(vi))

    {
      import syntax.functor._
      val voi2: Validation[String, Option[Int]] = vi.fpoint[Option]
      voi2 must be_===(success[String, Option[Int]](Some(0)))
    }
  }

  "show" in {
    import syntax.show._
    Validation.success[String, Int](0).shows must be_===("Success(0)")
    Validation.failure[String, Int]("fail").shows must be_===("Failure(fail)")
  }

  "example" in {
    import std.AllInstances._
    import syntax.functor._
    import std.string.stringSyntax._
    val x = "0".parseBoolean.fail.fpair
    ok
  }

  object instances {
    def show[E: Show, A: Show] = Show[Validation[E, A]]
    def equal[E: Equal, A: Equal] = Equal[Validation[E, A]]
    def order[E: Order, A: Order] = Order[Validation[E, A]]
    def pointed[E] = Pointed[({type λ[α]=Validation[E, α]})#λ]
    def semigroup[E: Semigroup, A] = Semigroup[Validation[E, A]]
    def applicative[E: Semigroup] = Applicative[({type λ[α]=Validation[E, α]})#λ]
    def traverse[E: Semigroup] = Traverse[({type λ[α]=Validation[E, α]})#λ]
    def plus[E: Semigroup] = Plus[({type λ[α]=Validation[E, α]})#λ]
    def bitraverse = BiTraverse[Validation]

    // checking absense of ambiguity
    def equal[E: Order, A: Order] = Equal[Validation[E, A]]
    def pointed[E: Semigroup] = Pointed[({type λ[α] = Validation[E, α]})#λ]

    object failProjection {
      def show[E: Show, A: Show] = Show[FailProjection[E, A]]
      def equal[E: Equal, A: Equal] = Equal[FailProjection[E, A]]
      def order[E: Order, A: Order] = Order[FailProjection[E, A]]
      def pointed[E] = Pointed[({type λ[α]=FailProjection[E, α]})#λ]
      def semigroup[E: Semigroup, A] = Semigroup[FailProjection[E, A]]
      def applicative[E: Semigroup] = Applicative[({type λ[α]=FailProjection[E, α]})#λ]
      def traverse[E: Semigroup] = Traverse[({type λ[α]=FailProjection[E, α]})#λ]
      def plus[E: Semigroup] = Plus[({type λ[α]=FailProjection[E, α]})#λ]
      def bitraverse = BiTraverse[FailProjection]

      // checking absense of ambiguity
      def equal[E: Order, A: Order] = Equal[FailProjection[E, A]]
      def pointed[E: Semigroup] = Pointed[({type λ[α]=FailProjection[E, α]})#λ]
    }
  }
}
