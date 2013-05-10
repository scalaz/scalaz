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

    val vi = Validation.success[String, Int](0)

    val voi: Validation[String, Option[Int]] = vi map (Some(_))
    val ovi: Option[Validation[String, Int]] = vi.point[Option]
    voi must be_===(Validation.success[String, Option[Int]](Some(0)))
    ovi must be_===(Some(vi))

    {
      import syntax.functor._
      val voi2: Validation[String, Option[Int]] = vi.fpoint[Option]
      voi2 must be_===(Validation.success[String, Option[Int]](Some(0)))
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
      val fail1 = Failure("1").toValidationNel
      val fail2 = Failure("2").toValidationNel
      val f = (_:Int) + (_:Int)
      Apply[({type l[a] = ValidationNel[String, a]})#l].ap2(fail1, fail2)(Success(f)).shows must be_===("""Failure(["1","2"])""")
    }
  }

  "map2" should {
    "accumulate failures in order" in {
      import syntax.show._
      val fail1 = Failure("1").toValidationNel
      val fail2 = Failure("2").toValidationNel
      val f = (_:Int) + (_:Int)
      Apply[({type l[a] = ValidationNel[String, a]})#l].apply2(fail1, fail2)(f).shows must be_===("""Failure(["1","2"])""")
    }
  }

  "findSuccess" should {
    import syntax.show._
    "accumulate failures in order" in {
      val fail1 = Validation.failure[String, Int]("1").toValidationNel
      val fail2 = Validation.failure[String, Int]("2").toValidationNel

      (fail1 findSuccess fail2).shows must be_===("""Failure(["1","2"])""")
    }

    "return first success" in {
      val succ = Validation.success[String, Int](1).toValidationNel
      val fail = Validation.failure[String, Int]("2").toValidationNel
    
      (succ findSuccess fail).shows must be_===(succ.shows)
      (fail findSuccess succ).shows must be_===(succ.shows)
    }
  }
    
  "excepting" in {
    import syntax.std.string._
    import syntax.validation._
    def errmsg(i: Int) = "Int must be positive: " + i
    (List("1", "2", "3") map (_.parseInt.leftMap(_.toString) excepting { case i if i < 0 => errmsg(i) })) must be_===(List(1.success[String], 2.success[String], 3.success[String]))

    (List("1", "-2", "3") map (_.parseInt.leftMap(_.toString) excepting { case i if i < 0 => errmsg(i) })) must be_===(List(1.success[String], errmsg(-2).fail[Int], 3.success[String]))

    implicit val ShowAny: Show[Any] = Show.showA; implicit val EqualAny: Equal[Any] = Equal.equalA
    def errmsgA(i: Int): Any = errmsg(i)
    (List("1", "2", "3") map (_.parseInt.leftMap(_.toString) excepting { case i if i < 0 => errmsgA(i) })) must be_===(List(1.success[Any], 2.success[Any], 3.success[Any]))

    (List("1", "-2", "3") map (_.parseInt.leftMap(_.toString) excepting { case i if i < 0 => errmsgA(i) })) must be_===(List(1.success[Any], errmsgA(-2).fail[Int], 3.success[Any]))
  }

  "ensure" in {
    import syntax.std.string._
    import syntax.validation._
    List("1", "2") map (_.parseInt.leftMap(_.toString).ensure("Fail")(_ >= 0)) must be_===(List(1.success[String], 2.success[String]))
    List("1", "-2") map (_.parseInt.leftMap(_.toString).ensure("Fail")(_ >= 0)) must be_===(List(1.success[String], "Fail".fail[Int]))
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
