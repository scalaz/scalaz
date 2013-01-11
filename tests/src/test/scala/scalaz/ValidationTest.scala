package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ValidationTest extends Spec {
  import std.AllInstances._

  checkAll("Validation", order.laws[Validation[Int, Int]])

  type ValidationInt[A] = Validation[Int, A]

  checkAll("Validation", semigroup.laws[ValidationInt[Int]])
  checkAll("Validation", monoid.laws[ValidationInt[Int]])
  checkAll("Validation", plus.laws[ValidationInt])
  checkAll("Validation", applicative.laws[ValidationInt])
  checkAll("Validation", traverse.laws[ValidationInt])
  checkAll("Validation", bifunctor.laws[Validation])
  checkAll("Validation", bitraverse.laws[Validation])

  "fpoint and point" in {

    import syntax.applicative._
    import std.AllInstances._
    import Validation._


    val vi = success[String, Int](0)

    val voi: Validation[String, Option[Int]] = vi map (Some(_))
    val ovi: Option[Validation[String, Int]] = vi.point[Option]
    voi must be_===(success[String, Option[Int]](Some(0)))
    ovi must be_===(Some(vi))

    {
      import syntax.functor._
      val voi2: Validation[String, Option[Int]] = vi.fpoint[Option]
      voi2 must be_===(success[String, Option[Int]](Some(0)))
      println("hi")
    }
  }

  "show" in {
    import syntax.show._
    Validation.success[String, Int](0).shows must be_===("Success(0)")
    Validation.failure[String, Int]("fail").shows must be_===("Failure(\"fail\")")
  }

  "ap2" should {
    "accumulate failures in order" in {
      import syntax.show._
      val fail1 = Failure("1").toValidationNEL
      val fail2 = Failure("2").toValidationNEL
      val f = (_:Int) + (_:Int)
      Apply[({type l[a] = ValidationNEL[String, a]})#l].ap2(fail1, fail2)(Success(f)).shows must be_===("""Failure(["1","2"])""")
    }
  }

  "map2" should {
    "accumulate failures in order" in {
      import syntax.show._
      val fail1 = Failure("1").toValidationNEL
      val fail2 = Failure("2").toValidationNEL
      val f = (_:Int) + (_:Int)
      Apply[({type l[a] = ValidationNEL[String, a]})#l].apply2(fail1, fail2)(f).shows must be_===("""Failure(["1","2"])""")
    }
  }

  "findSuccess" should {
    import syntax.show._
    "accumulate failures in order" in {
      val fail1 = Validation.failure[String, Int]("1").toValidationNEL
      val fail2 = Validation.failure[String, Int]("2").toValidationNEL

      (fail1 findSuccess fail2).shows must be_===("""Failure(["1","2"])""")
    }

    "return first success" in {
      val succ = Validation.success[String, Int](1).toValidationNEL
      val fail = Validation.failure[String, Int]("2").toValidationNEL
    
      (succ findSuccess fail).shows must be_===(succ.shows)
      (fail findSuccess succ).shows must be_===(succ.shows)
    }
  }

  object instances {
    def show[E: Show, A: Show] = Show[Validation[E, A]]
    def equal[E: Equal, A: Equal] = Equal[Validation[E, A]]
    def order[E: Order, A: Order] = Order[Validation[E, A]]
    def semigroup[E: Semigroup, A: Semigroup] = Semigroup[Validation[E, A]]
    def applicative[E: Semigroup] = Applicative[({type λ[α]=Validation[E, α]})#λ]
    def traverse[E: Semigroup] = Traverse[({type λ[α]=Validation[E, α]})#λ]
    def plus[E: Semigroup] = Plus[({type λ[α]=Validation[E, α]})#λ]
    def bitraverse = Bitraverse[Validation]

    // checking absence of ambiguity
    def equal[E: Order, A: Order] = Equal[Validation[E, A]]
  }
}
