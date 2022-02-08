package scalaz.example

object SyntaxUsage {

  val o1: Option[Int] = Some(0)
  val o2: Option[Option[Int]] = Some(Some(0))
  val l1: List[String] = List("one")
  val l2: List[List[String]] = List(List("one"))

  def main(args: Array[String]): Unit = {
    syntax1()
    syntax2()
    syntax3()
    stdSyntax()
    stdSyntaxUeber()
  }

  def typed[T](t: T) = ()

  // Use the syntax only for Monad[Option]
  // This includes the syntax for the parent type classes.
  def syntax1(): Unit = {
    import scalaz._

    // Import type class instances for Option, and the
    // Monad syntax for Option.
    import std.option._
    import std.option.optionInstance.monadSyntax._
    val x = 1.point
    val y = point(1)
    typed[Option[Int]](x)
    typed[Option[Int]](x)

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
  }

  // Use two different instances, and the syntax for all Monads
  def syntax2(): Unit = {
    import scalaz._

    // Import type class instances for Option and List
    import std.option._
    import std.list._

    // Implicit conversions from M[A] => BindV[M, A], etc.
    import syntax.monad._

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
    l2.join

    1.point[Option]
  }

  def syntax3(): Unit = {
    import scalaz._

    // Import all type class instances
    import Scalaz._

    o1 >>= (x => if (x == 0) Some(0) else None)
    o2.join
    l2.join

    o2.tuple(o2)
  }

  def stdSyntax(): Unit = {
    import scalaz.Tags.Last
    import scalaz.std.anyVal._
    import scalaz.std.stream.streamSyntax._

    val interleaved = Stream(1, 3, 5).interleave(Stream(2, 4, 6))

    import scalaz.std.list._
    import scalaz.std.option._
    import scalaz.std.option.optionSyntax._
    import scalaz.syntax.monoid._
    import scalaz.syntax.equal._

    val lists: List[Int] = some(1).orEmpty[List]
    Last.unwrap((some(1).last |+| some(2).last)) assert_=== some(2)
  }

  def stdSyntaxUeber(): Unit = {
    // Scalaz 6 Style: import everything: type class instances, implicit conversions
    // to the syntax wrappers, general functions.
    import scalaz._
    import scalaz.Scalaz._

    // prefix style function call `op(args)`
    orEmpty[Int, List](some(1))

    // syntax to provide `x.op(args)`
    some(1).orEmpty[List]

    Tag.unwrap((some(1).last |+| some(2).last)) assert_=== some(2)
    some(some(1)).join assert_=== some(1)

    List(1, 2, 3).powerset.join

    import Kleisli._

    val k = kleisli((a: Int) => some(0))
    kleisliArrow[Option].compose(k, k)
    k >>> k

    List(some(0)).sequence
  }

  def unambiguousFunctorSyntax(): Unit = {
    import scalaz._

    def foo[F[_]: Monad: Traverse, A, B](fa: F[A], f: A => B): F[B] = {
      // Importing Functor syntax (import scalaz.syntax.functor._) would
      // give ambiguous implicit values for Functor[F] when trying to use
      // Functor syntax.
      // Monad syntax however requires a Monad even for functor operations,
      // and there is a unique Monad[F] in scope.
      import scalaz.syntax.monad._

      fa map f
    }
  }

  def mixingMonadAndTraverseSyntax(): Unit = {
    import scalaz._

    def foo[F[_]: Monad: Traverse, A, B](fa: F[A], f: A => B): Unit = {
      // Here we want to use both Monad and Traverse syntax, as well as
      // Functor syntax. Since F is both Monad and Traverse, there are
      // two ways to get Functor syntax for F. To avoid ambiguity,
      // import Traverse syntax that doesn't include Functor syntax (traverse0).
      import scalaz.syntax.monad._
      import scalaz.syntax.traverse0._

      // Functor syntax
      fa map f

      // Monad syntax
      fa flatMap (a => f(a).point[F])

      // Traverse and Applicative syntax
      fa traverse (a => f(a).point[F])
    }
  }

  def mixingApplicativeAndBindSyntax(): Unit = {
    import scalaz._

    def foo[F[_]: Applicative: Bind, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      // To avoid ambiguity, import Applicative syntax that
      // doesn't include Apply syntax (applicative0).
      import scalaz.syntax.bind._
      import scalaz.syntax.applicative0._

      // Bind syntax
      fa flatMap (_ => fb)

      // Applicative syntax
      1.point[F]

      // Apply syntax
      (fa |@| fb).tupled
    }
  }

  def mixingMonadTellAndMonadErrorSyntax(): Unit = {
    import scalaz._

    def foo[F[_], E](fa: F[Int])(implicit mt: MonadTell[F, String], me: MonadError[F, E]) = {
      // To avoid ambiguity, import MonadError syntax that
      // doesn't include Monad syntax (monadError0).
      import scalaz.syntax.monadTell._
      import scalaz.syntax.monadError0._
      import scalaz.syntax.applicativeError0._
      // Monad syntax
      fa flatMap (_ => fa)

      // MonadTell syntax
      fa :++> "foo"

      // ApplicativeError syntax
      fa handleError (e => fa)
    }
  }
}
