package scalaz

import std.AllInstances._

class OptionalTest extends Spec {

  def definedTests[F[_],A](context: F[A], value: A, default: => A, alternative: => F[A])(implicit O: Optional[F], EA: Equal[A], EFA: Equal[F[A]], SA: Show[A], SFA: Show[F[A]]) = {
    O.getOrElse(context)(default)  must be_===(value)
    O.isDefined(context)           must be_===(true)
    O.orElse(context)(alternative) must be_===(context)
    O.nonEmpty(context)            must be_===(true)
    O.isEmpty(context)             must be_===(false)
    O.?(context)(1,0)              must be_===(1)
    O.toOption(context)            must be_===(Option(value))
  }

  def undefinedTests[F[_],A](context: F[A], default: A, alternative: F[A])(implicit O: Optional[F], EA: Equal[A], EFA: Equal[F[A]], SA: Show[A], SFA: Show[F[A]]) = {
    O.getOrElse(context)(default)  must be_===(default)
    O.isDefined(context)           must be_===(false)
    O.orElse(context)(alternative) must be_===(alternative)
    O.nonEmpty(context)            must be_===(false)
    O.isEmpty(context)             must be_===(true)
    O.?(context)(1,0)              must be_===(0)
    O.toOption(context)            must be_===(Option.empty[A])
  }

  """\/ instance tests""" in {
    type EitherInt[A] = Int \/ A

    def right(a: Int): EitherInt[Int] = \/.right(a)
    def left(a: Int): EitherInt[Int] = \/.left(a)

    definedTests(right(1), 1, 0, right(0))
    undefinedTests(left(0), 0, right(0))
  }

  // LazyOption[Show[_]] is currently unavailable
//  "LazyOption instance tests" in {
//    definedTests(LazyOption.lazySome(1), 1, 0, LazyOption.lazySome(0))
//    undefinedTests(LazyOption.lazyNone[Int], 0, LazyOption.lazySome(0))
//  }

  "Option instance tests" in {
    definedTests(Option(1), 1, 0, Option(0))
    undefinedTests(Option.empty[Int], 0, Option(0))
  }

  "syntax test" in {
    import syntax.optional._

    val value = Option(1)

    ( value.getOrElse(0)      ) must be_===(1)
    ( value.isDefined         ) must be_===(true)
    ( value.orElse(Option(0)) ) must be_===(value)
    ( value.nonEmpty          ) must be_===(true)
    ( value.isEmpty           ) must be_===(false)
    ( value ? 1 | 0           ) must be_===(1)
    ( value.toOption          ) must be_===(value)
  }

}
