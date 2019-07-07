package scalaz

import java.util.concurrent.atomic.AtomicInteger

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object EitherTTest extends SpecLite {

  type EitherTList[A, B] = EitherT[A, List, B]
  type EitherTListInt[A] = EitherT[Int, List, A]
  type EitherTOptionInt[A] = EitherT[Int, Option, A]
  type EitherTComputation[A] = EitherT[Int, Function0, A] // in lieu of IO

  checkAll(equal.laws[EitherTListInt[Int]])
  checkAll(bindRec.laws[EitherTListInt])
  checkAll(monadPlus.laws[EitherTListInt])
  checkAll(monadError.laws[EitherTListInt, Int])
  checkAll(traverse.laws[EitherTListInt])
  checkAll(bitraverse.laws[EitherTList])
  checkAll(monadTrans.laws[EitherT[Int, ?[_], ?], List])
  checkAll(alt.laws[EitherTListInt])

  "rightU" should {
    val a: String \/ Int = \/-(1)
    val b: EitherT[Boolean, ({type l[a] = String \/ a})#l, Int] = EitherT.rightU[Boolean](a)
    b must_== EitherT.rightT[Boolean, ({type l[a] = String \/ a})#l, Int](a)
  }

  "consistent Bifoldable" ! forAll { a: EitherTList[Int, Int] =>
    val F = new Bitraverse[EitherTList]{
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: EitherTList[A, B])(f: A => G[C], g: B => G[D]) =
        EitherT.eitherTBitraverse[List].bitraverseImpl(fab)(f, g)
    }

    Bifoldable[EitherTList].bifoldMap(a)(_ :: Nil)(_ :: Nil) must_=== F.bifoldMap(a)(_ :: Nil)(_ :: Nil)
  }

  "show" ! forAll { a: EitherTList[Int, Int] =>
    Show[EitherTList[Int, Int]].show(a) must_=== Show[List[Int \/ Int]].show(a.run)
  }

  "fromDisjunction" ! forAll { (a: String \/ Int) =>
    Option(a.isLeft) must_=== EitherT.fromDisjunction[Option](a).isLeft
  }

  "fromOption" ! forAll { (o: Option[String], s: String) =>
    List(o.isEmpty) must_=== EitherT.fromOption(s)(List(o)).isLeft
  }

  "either, left, right" ! forAll { (a: String \/ Int) =>
    val e = EitherT.eitherT(Option(a))

    e must_=== {
      a match {
        case -\/(v) => EitherT.left(v)
        case \/-(v) => EitherT.right(v)
      }
    }

    e must_=== EitherT.either(a)
  }

  "flatMapF consistent with flatMap" ! forAll { (a: EitherTList[Int, Int], f: Int => List[Int \/ String]) =>
    a.flatMap(f andThen EitherT.apply) must_=== a.flatMapF(f)
  }

  "mapF consistent with map" ! forAll { (a: EitherTList[Int, Int], f: Int => String) =>
    a.map(f) must_=== a.mapF(f andThen (s => Applicative[List].point(s)))
  }


  "orElse only executes the left hand monad once" should {
    val counter = new AtomicInteger(0)
    val inc: EitherTComputation[Int] = EitherT.rightT(() => counter.incrementAndGet())
    val other: EitherTComputation[Int] = EitherT.rightT(() => 0) // does nothing

    (inc orElse other).run.apply() must_== \/-(1)
    counter.get() must_== 1
  }

  object instances {
    def functor[A, F[_] : Functor] = Functor[EitherT[A, F, ?]]
    def bindRec[A, F[_] : Monad: BindRec] = BindRec[EitherT[A, F, ?]]
    def monad[A, F[_] : Monad] = Monad[EitherT[A, F, ?]]
    def plus[A: Semigroup, F[_] : Monad] = Plus[EitherT[A, F, ?]]
    def monadPlus[A: Monoid, F[_] : Monad] = MonadPlus[EitherT[A, F, ?]]
    def foldable[A, F[_] : Foldable] = Foldable[EitherT[A, F, ?]]
    def traverse[A, F[_] : Traverse] = Traverse[EitherT[A, F, ?]]
    def bifunctor[F[_] : Functor] = Bifunctor[EitherT[?, F, ?]]
    def bifoldable[F[_] : Foldable] = Bifoldable[EitherT[?, F, ?]]
    def bitraverse[F[_] : Traverse] = Bitraverse[EitherT[?, F, ?]]
    def monadReader[E, R, F[_]](implicit F0: MonadReader[F, R]) = MonadReader[EitherT[E, F, ?], R]

    // checking absence of ambiguity
    def functor[A, F[_] : BindRec] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Monad] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Monad : BindRec] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Traverse] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Monad: Traverse] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : BindRec: Traverse] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Monad: BindRec: Traverse] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Nondeterminism] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Monad : Nondeterminism] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : BindRec : Nondeterminism] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Monad : BindRec : Nondeterminism] = Functor[EitherT[A, F, ?]]
    def functor[A, F[_] : Traverse : Nondeterminism] = Functor[EitherT[A, F, ?]]
    def functor[A: Monoid, F[_] : Monad] = Functor[EitherT[A, F, ?]]
    def functor[A: Monoid, F[_] : Monad : BindRec] = Functor[EitherT[A, F, ?]]
    def functor[A: Monoid, F[_] : Monad : Traverse] = Functor[EitherT[A, F, ?]]
    def functor[A: Monoid, F[_] : Monad : Nondeterminism] = Functor[EitherT[A, F, ?]]
    def apply[A: Monoid, F[_] : Monad] = Apply[EitherT[A, F, ?]]
    def monad[A: Monoid, F[_] : Monad] = Monad[EitherT[A, F, ?]]
    def plus[A: Monoid, F[_] : Monad] = Plus[EitherT[A, F, ?]]
    def foldable[A, F[_] : Traverse] = Foldable[EitherT[A, F, ?]]
    def bifunctor[F[_] : Traverse] = Bifunctor[EitherT[?, F, ?]]
    def bifoldable[F[_] : Traverse] = Bifoldable[EitherT[?, F, ?]]
    def monadError[A, F[_] : Monad] = MonadError[EitherT[A, F, ?], A]
    def nondeterminism[A, F[_] : Nondeterminism] = Nondeterminism[EitherT[A, F, ?]]
    def nondeterminismMonad[A, F[_] : Nondeterminism] = Monad[EitherT[A, F, ?]]
    def nondeterminismFunctor[A, F[_] : Nondeterminism: BindRec: Traverse] = Functor[EitherT[A, F, ?]]
    def nondeterminismMonad[A, F[_] : Nondeterminism: BindRec: Traverse] = Monad[EitherT[A, F, ?]]

    def monadReader[E, R, F[_]](implicit F0: MonadReader[F, R]) = Monad[EitherT[E, F, ?]]
  }

  def compilationTests() = {
    // compilation test
    // https://gist.github.com/vmarquez/5106252/
    {
      import scalaz.syntax.either._

      case class ABC(s:String)

      implicit val m = new Monoid[(ABC, Int)] {
        def zero: (ABC, Int) = (null, -1)
        def append(f1: (ABC, Int), f2: => (ABC, Int)): (ABC, Int) = f1
      }

      def brokenMethod: EitherT[(ABC, Int), Option, (ABC, String)] =
        EitherT(Some((ABC("abcData"),"Success").right))

      def filterComp =
        brokenMethod
        .filter {
          case (abc,"Success") => true
          case _ => false
        }.map {
          case (abc, "Success") => "yay"
        }

      for {
        (a,b) <- brokenMethod
      } yield "yay"
    }

    //compilation test for eitherTU
    {
      val se: State[Vector[String], Int \/ Float] = null
      EitherT.eitherTU(se)
      val ee: String \/ (Int \/ Float) = null
      EitherT.eitherTU(ee)
    }
  }
}
