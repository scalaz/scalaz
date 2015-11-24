package scalaz

import Scalaz.Id
import std.AllInstances._

object OptionalTest extends SpecLite {

  def definedTests[F[_],A](context: F[A], value: A, default: => A, alternative: => F[A])(implicit O: Optional[F], EA: Equal[A], EFA: Equal[F[A]], SA: Show[A], SFA: Show[F[A]]) = {
    O.getOrElse(context)(default)  must_===(value)
    O.isDefined(context)           must_===(true)
    O.orElse(context)(alternative) must_===(context)
    O.nonEmpty(context)            must_===(true)
    O.isEmpty(context)             must_===(false)
    O.?(context)(1,0)              must_===(1)
    O.toOption(context)            must_===(Option(value))
    O.toMaybe(context)             must_===(Maybe.just(value))
  }

  def undefinedTests[F[_],A](context: F[A], default: A, alternative: F[A])(implicit O: Optional[F], EA: Equal[A], EFA: Equal[F[A]], SA: Show[A], SFA: Show[F[A]]) = {
    O.getOrElse(context)(default)  must_===(default)
    O.isDefined(context)           must_===(false)
    O.orElse(context)(alternative) must_===(alternative)
    O.nonEmpty(context)            must_===(false)
    O.isEmpty(context)             must_===(true)
    O.?(context)(1,0)              must_===(0)
    O.toOption(context)            must_===(Option.empty[A])
    O.toMaybe(context)             must_===(Maybe.empty[A])
  }

  """\/ instance tests""" in {
    type EitherInt[A] = Int \/ A

    def right(a: Int): EitherInt[Int] = \/.right(a)
    def left(a: Int): EitherInt[Int] = \/.left(a)

    definedTests(right(1), 1, 0, right(0))
    undefinedTests(left(0), 0, right(0))
  }

  // currently there are no Equal[LazyEither], Show[LazyEither]
//  """LazyEither instance tests""" in {
//    type LEInt[A] = LazyEither[Int,A]
//    def right(a: Int): LEInt[Int] = LazyEither.lazyRight(a)
//    def left(a: Int): LEInt[Int] = LazyEither.lazyLeft(a)
//    definedTests(right(1), 1, 0, right(0))
//    undefinedTests(left(0), 0, right(0))
//  }

  // currently there is no Show[LazyOption]
//  "LazyOption instance tests" in {
//    definedTests(LazyOption.lazySome(1), 1, 0, LazyOption.lazySome(0))
//    undefinedTests(LazyOption.lazyNone[Int], 0, LazyOption.lazySome(0))
//  }

  "Option instance tests" in {
    definedTests(Option(1), 1, 0, Option(0))
    undefinedTests(Option.empty[Int], 0, Option(0))
  }

  """Validation instance tests""" in {
    type VString[A] = Validation[String,A]

    def success(a: Int): VString[Int] = Validation.success(a)
    def failure(s: String): VString[Int] = Validation.failure(s)

    definedTests[VString, Int](success(1), 1, 0, success(0))
    undefinedTests[VString, Int](failure("oO"), 0, success(0))
  }

  """ValidationNel instance tests""" in {
    type VStringNel[A] = ValidationNel[String, A]

    def successNel(a: Int): VStringNel[Int] = Validation.success(a)
    def failureNel(s: String): VStringNel[Int] = Validation.failureNel(s)

    definedTests[VStringNel, Int](successNel(1), 1, 0, successNel(0))
    undefinedTests[VStringNel, Int](failureNel("oO"), 0, successNel(0))
  }

  "Id instance tests" in {
    definedTests[Id, Int](1, 1, 0, 0)
  }

  "Maybe instance tests" in {
    definedTests(Maybe.just(1), 1, 0, Maybe.just(0))
    undefinedTests(Maybe.empty[Int], 0, Maybe.just(0))
  }

  "syntax test" in {
    import syntax.optional._

    val value = Option(1)

    ( value.getOrElse(0)      ) must_===(1)
    ( value.isDefined         ) must_===(true)
    ( value.orElse(Option(0)) ) must_===(value)
    ( value.nonEmpty          ) must_===(true)
    ( value.isEmpty           ) must_===(false)
    ( value ? 1 | 0           ) must_===(1)
    ( value.toOption          ) must_===(value)
  }

}
