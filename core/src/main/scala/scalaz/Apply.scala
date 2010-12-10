package scalaz

trait Apply[Z[_]] {
  def apply[A, B](f: Z[A => B], a: Z[A]): Z[B]
}

trait Applys {
  def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]) = new Apply[Z] {
    def apply[A, B](f: Z[A => B], a: Z[A]): Z[B] = {
      lazy val fv = f
      lazy val fa = a
      b.bind(fv, (g: A => B) => t.fmap(fa, g(_: A)))
    }
  }
}

abstract class ApplyLow {
  implicit def FunctorBindApply[Z[_]](implicit t: Functor[Z], b: Bind[Z]): Apply[Z] = Scalaz.FunctorBindApply(t, b)
}

object Apply extends ApplyLow {
  import Scalaz._

  implicit def ConstApply[B: Monoid] = new Apply[({type λ[α]=Const[B, α]})#λ] {
    def apply[A, X](f: Const[B, A => X], fa: Const[B, A]) = {
      lazy val fv = f;
      lazy val fav = fa;
      Const[B, X](fv.value ⊹ fav.value)
    }
  }

  implicit def StateApply[S]: Apply[({type λ[α]=State[S, α]})#λ] = FunctorBindApply[({type λ[α]=State[S, α]})#λ]

  implicit def Tuple2Apply[R: Monoid]: Apply[({type λ[α]=(R, α)})#λ] = FunctorBindApply[({type λ[α]=(R, α)})#λ]

  implicit def Tuple3Apply[R: Monoid, S: Monoid]: Apply[({type λ[α]=(R, S, α)})#λ] = FunctorBindApply[({type λ[α]=(R, S, α)})#λ]

  implicit def Tuple4Apply[R: Monoid, S: Monoid, T: Monoid]: Apply[({type λ[α]=(R, S, T, α)})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, α)})#λ]

  implicit def Tuple5Apply[R: Monoid, S: Monoid, T: Monoid, U: Monoid]: Apply[({type λ[α]=(R, S, T, U, α)})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, U, α)})#λ]

  implicit def Tuple6Apply[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid]: Apply[({type λ[α]=(R, S, T, U, V, α)})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, U, V, α)})#λ]

  implicit def Tuple7Apply[R: Monoid, S: Monoid, T: Monoid, U: Monoid, V: Monoid, W: Monoid]: Apply[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, U, V, W, α)})#λ]
    
  implicit def Function1Apply[R]: Apply[({type λ[α]=(R) => α})#λ] = FunctorBindApply[({type λ[α]=(R) => α})#λ]

  implicit def Function2Apply[R, S]: Apply[({type λ[α]=(R, S) => α})#λ] = FunctorBindApply[({type λ[α]=(R, S) => α})#λ]

  implicit def Function3Apply[R, S, T]: Apply[({type λ[α]=(R, S, T) => α})#λ] = FunctorBindApply[({type λ[α]=(R, S, T) => α})#λ]

  implicit def Function4Apply[R, S, T, U]: Apply[({type λ[α]=(R, S, T, U) => α})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, U) => α})#λ]

  implicit def Function5Apply[R, S, T, U, V]: Apply[({type λ[α]=(R, S, T, U, V) => α})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, U, V) => α})#λ]

  implicit def Function6Apply[R, S, T, U, V, W]: Apply[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = FunctorBindApply[({type λ[α]=(R, S, T, U, V, W) => α})#λ]

  implicit def EitherLeftApply[X]: Apply[({type λ[α]=Either.LeftProjection[α, X]})#λ] = FunctorBindApply[({type λ[α]=Either.LeftProjection[α, X]})#λ]

  implicit def EitherRightApply[X]: Apply[({type λ[α]=Either.RightProjection[X, α]})#λ] = FunctorBindApply[({type λ[α]=Either.RightProjection[X, α]})#λ]

  import java.util.Map.Entry

  implicit def MapEntryApply[X: Semigroup]: Apply[({type λ[α]=Entry[X, α]})#λ] = FunctorBindApply[({type λ[α]=Entry[X, α]})#λ]

  implicit def ValidationApply[X: Semigroup]: Apply[({type λ[α]=Validation[X, α]})#λ] = new Apply[({type λ[α]=Validation[X, α]})#λ] {
    def apply[A, B](f: Validation[X, A => B], a: Validation[X, A]) = (f, a) match {
      case (Success(f), Success(a)) => success(f(a))
      case (Success(_), Failure(e)) => failure(e)
      case (Failure(e), Success(_)) => failure(e)
      case (Failure(e1), Failure(e2)) => failure(e1 ⊹ e2)
    }
  }

  implicit def ValidationFailureApply[X]: Apply[({type λ[α]=FailProjection[α, X]})#λ] = new Apply[({type λ[α]=FailProjection[α, X]})#λ] {
    def apply[A, B](f: FailProjection[A => B, X], a: FailProjection[A, X]) = ((f.validation, a.validation) match {
      case (Success(x1), Success(_)) => success(x1)
      case (Success(x1), Failure(_)) => success(x1)
      case (Failure(_), Success(x2)) => success(x2)
      case (Failure(f), Failure(e)) => failure(f(e))
    }).fail
  }

  implicit def ZipperApply: Apply[Zipper] = new Apply[Zipper] {
    def apply[A, B](f: Zipper[A => B], a: Zipper[A]): Zipper[B] =
      zipper((a.lefts ʐ) <*> (f.lefts ʐ),
        (f.focus)(a.focus),
        (a.rights ʐ) <*> (f.rights ʐ))
  }

  implicit def ZipStreamApply: Apply[ZipStream] = new Apply[ZipStream] {
    def apply[A, B](f: ZipStream[A => B], a: ZipStream[A]): ZipStream[B] = {
      val ff = f.value
      val aa = a.value
      (if (ff.isEmpty || aa.isEmpty) Stream.empty
      else Stream.cons((ff.head)(aa.head), apply(ff.tail ʐ, aa.tail ʐ))) ʐ
    }
  }

  val ZipTreeApply: Apply[Tree] = new Apply[Tree] {
    def apply[A, B](f: Tree[A => B], a: Tree[A]): Tree[B] =
      node((f.rootLabel)(a.rootLabel), (a.subForest ʐ) <*> (f.subForest.map((apply(_: Tree[A => B], _: Tree[A])).curried) ʐ))
  }

  import concurrent.Promise
  implicit val PromiseApply = FunctorBindApply[Promise]

  import java.util._
  import java.util.concurrent._

  implicit val JavaArrayListApply = FunctorBindApply[ArrayList]

  implicit val JavaLinkedListApply = FunctorBindApply[LinkedList]

  implicit val JavaPriorityQueueApply = FunctorBindApply[PriorityQueue]

  implicit val JavaStackApply = FunctorBindApply[Stack]

  implicit val JavaVectorApply = FunctorBindApply[Vector]

  implicit val JavaArrayBlockingQueueApply = FunctorBindApply[ArrayBlockingQueue]

  implicit val JavaConcurrentLinkedQueueApply = FunctorBindApply[ConcurrentLinkedQueue]

  implicit val JavaCopyOnWriteArrayListApply = FunctorBindApply[CopyOnWriteArrayList]

  implicit val JavaLinkedBlockingQueueApply = FunctorBindApply[LinkedBlockingQueue]

  implicit val JavaSynchronousQueueApply = FunctorBindApply[SynchronousQueue]
}
