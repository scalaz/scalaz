package scalaz

import java.util.Map.Entry
import java.util.AbstractMap.SimpleImmutableEntry

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]

  def compose[G[_]](gtr: Functor[G]): Functor[({type λ[α] = F[G[α]]})#λ] = new Functor[({type λ[α] = F[G[α]]})#λ] {
    def fmap[A, B](f: A => B): F[G[A]] => F[G[B]] =
      Functor.this.fmap(gtr.fmap(f))
  }

  def **[G[_] : Functor]: Functor[({type λ[α] = (F[α], G[α])})#λ] =
    new Functor[({type λ[α] = (F[α], G[α])})#λ] {
      def fmap[A, B](f: A => B) = {
        case (a, b) => (Functor.this.fmap(f)(a), implicitly[Functor[G]].fmap(f)(b))
      }
    }

  def deriving[G[_]](implicit n: ^**^[G, F]): Functor[G] =
    new Functor[G] {
      def fmap[A, B](f: A => B) =
        k => n.pack(Functor.this.fmap(f)(n.unpack(k)))
    }
}

object Functor extends Functors

trait Functors extends FunctorsLow {

  implicit val OptionFunctor: Functor[Option] = new Functor[Option] {
    def fmap[A, B](f: A => B) = _ map f
  }

  implicit def EitherLeftFunctor[X]: Functor[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Functor[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def fmap[A, B](f: A => B) =
      _.map(f).left
  }

  implicit def EitherRightFunctor[X]: Functor[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Functor[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def fmap[A, B](f: A => B) = _.map(f).right
  }

  implicit def EitherFunctor[X]: Functor[({type λ[α]=Either[X, α]})#λ] = new Functor[({type λ[α]=Either[X, α]})#λ] {
    def fmap[A, B](f: A => B) = {
      case Left(a) => Left(a)
      case Right(a) => Right(f(a))
    }
  }

  implicit val ListFunctor: Functor[List] = new Functor[List] {
    def fmap[A, B](f: A => B) = _ map f
  }

  implicit val StreamFunctor: Functor[Stream] = new Functor[Stream] {
    def fmap[A, B](f: A => B) = _ map f
  }

  import java.util.concurrent.Callable

  implicit def CallableFunctor: Functor[Callable] = new Functor[Callable] {
    def fmap[A, B](f: A => B) =
      r => new Callable[B] {
        def call = f(r.call)
      }
  }

  implicit def MapEntryFunctor[X]: Functor[({type λ[α] = Entry[X, α]})#λ] = new Functor[({type λ[α] = Entry[X, α]})#λ] {
    def fmap[A, B](f: A => B) = r => new SimpleImmutableEntry(r.getKey, f(r.getValue))
  }

  implicit def Tuple1Functor: Functor[Tuple1] = new Functor[Tuple1] {
    def fmap[A, B](f: A => B) =
      r => Tuple1(f(r._1))
  }

  implicit def Tuple2Functor[R]: Functor[({type λ[α]=(R, α)})#λ] = new Functor[({type λ[α]=(R, α)})#λ] {
    def fmap[A, B](f: A => B) =
      r => (r._1, f(r._2))
  }

  implicit def Tuple3Functor[R, S]: Functor[({type λ[α]=(R, S, α)})#λ] = new Functor[({type λ[α]=(R, S, α)})#λ] {
    def fmap[A, B](f: A => B) =
      r => (r._1, r._2, f(r._3))
  }

  implicit def Tuple4Functor[R, S, T]: Functor[({type λ[α]=(R, S, T, α)})#λ] = new Functor[({type λ[α]=(R, S, T, α)})#λ] {
    def fmap[A, B](f: A => B) =
      r => (r._1, r._2, r._3, f(r._4))
  }

  implicit def Tuple5Functor[R, S, T, U]: Functor[({type λ[α]=(R, S, T, U, α)})#λ] = new Functor[({type λ[α]=(R, S, T, U, α)})#λ] {
    def fmap[A, B](f: A => B) =
      r => (r._1, r._2, r._3, r._4, f(r._5))
  }

  implicit def Tuple6Functor[R, S, T, U, V]: Functor[({type λ[α]=(R, S, T, U, V, α)})#λ] = new Functor[({type λ[α]=(R, S, T, U, V, α)})#λ] {
    def fmap[A, B](f: A => B) =
      r => (r._1, r._2, r._3, r._4, r._5, f(r._6))
  }

  implicit def Tuple7Functor[R, S, T, U, V, W]: Functor[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = new Functor[({type λ[α]=(R, S, T, U, V, W, α)})#λ] {
    def fmap[A, B](f: A => B) =
      r => (r._1, r._2, r._3, r._4, r._5, r._6, f(r._7))
  }

  implicit def Function0Functor: Functor[Function0] = new Functor[Function0] {
    def fmap[A, B](f: A => B) = r => new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit def Function1Functor[R]: Functor[({type λ[α]=(R) => α})#λ] = new Functor[({type λ[α]=(R) => α})#λ] {
    def fmap[A, B](f: A => B) = _ andThen f
  }

  implicit def Function2Functor[R, S]: Functor[({type λ[α]=(R, S) => α})#λ] = new Functor[({type λ[α]=(R, S) => α})#λ] {
    def fmap[A, B](f: A => B) = r => (t1: R, t2: S) => f(r(t1, t2))
  }

  implicit def Function3Functor[R, S, T]: Functor[({type λ[α]=(R, S, T) => α})#λ] = new Functor[({type λ[α]=(R, S, T) => α})#λ] {
    def fmap[A, B](f: A => B) = r => (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))
  }

  implicit def Function4Functor[R, S, T, U]: Functor[({type λ[α]=(R, S, T, U) => α})#λ] = new Functor[({type λ[α]=(R, S, T, U) => α})#λ] {
    def fmap[A, B](f: A => B) = r => (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))
  }

  implicit def Function5Functor[R, S, T, U, V]: Functor[({type λ[α]=(R, S, T, U, V) => α})#λ] = new Functor[({type λ[α]=(R, S, T, U, V) => α})#λ] {
    def fmap[A, B](f: A => B) = r => (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))
  }

  implicit def Function6Functor[R, S, T, U, V, W]: Functor[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = new Functor[({type λ[α]=(R, S, T, U, V, W) => α})#λ] {
    def fmap[A, B](f: A => B) = r => (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))
  }

  // todo use this rather than all the specific java.util._ Functor instances once the scala bug is fixed.
  // http://lampsvn.epfl.ch/trac/scala/ticket/2782
  /*implicit*/
  def JavaCollectionFunctor[S[X] <: java.util.Collection[X] : Empty]: Functor[S] = new Functor[S] {
    def fmap[A, B](f: A => B) =
    r => {
      val a: S[B] = implicitly[Empty[S]].empty
      val i = r.iterator
      while (i.hasNext)
        a.add(f(i.next))
      a
    }
  }

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListFunctor: Functor[ArrayList] = JavaCollectionFunctor

  implicit def JavaLinkedListFunctor: Functor[LinkedList] = JavaCollectionFunctor

  implicit def JavaPriorityQueueFunctor: Functor[PriorityQueue] = JavaCollectionFunctor

  implicit def JavaStackFunctor: Functor[Stack] = JavaCollectionFunctor

  implicit def JavaVectorFunctor: Functor[Vector] = JavaCollectionFunctor

  implicit def JavaArrayBlockingQueueFunctor: Functor[ArrayBlockingQueue] = JavaCollectionFunctor

  implicit def JavaConcurrentLinkedQueueFunctor: Functor[ConcurrentLinkedQueue] = JavaCollectionFunctor

  implicit def JavaCopyOnWriteArrayListFunctor: Functor[CopyOnWriteArrayList] = JavaCollectionFunctor

  implicit def JavaLinkedBlockingQueueFunctor: Functor[LinkedBlockingQueue] = JavaCollectionFunctor

  implicit def JavaSynchronousQueueFunctor: Functor[SynchronousQueue] = JavaCollectionFunctor

  implicit val IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](f: A => B) = a => Identity.id(f(a.value))
  }

  implicit def CoStateFunctor[A, F[_] : Functor]: Functor[({type λ[α] = CoStateT[A, F, α]})#λ] = new Functor[({type λ[α] = CoStateT[A, F, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def KleisliFunctor[F[_], R](implicit ftr: Functor[F]): Functor[({type λ[α] = Kleisli[R, F, α]})#λ] =
    new Functor[({type λ[α] = Kleisli[R, F, α]})#λ] {
      def fmap[A, B](f: A => B) =
        _ map f
    }

  implicit val NonEmptyListFunctor: Functor[NonEmptyList] = new Functor[NonEmptyList] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def StateTFunctor[A, F[_] : Functor]: Functor[({type λ[α] = StateT[A, F, α]})#λ] = new Functor[({type λ[α] = StateT[A, F, α]})#λ] {
    def fmap[X, Y](f: X => Y) =
      _ map f
  }

  implicit def StepListTFunctor[F[_] : Functor]: Functor[({type λ[X] = StepListT[F, X]})#λ] = new Functor[({type λ[X] = StepListT[F, X]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def StepStreamTFunctor[F[_] : Functor]: Functor[({type λ[X] = StepStreamT[F, X]})#λ] = new Functor[({type λ[X] = StepStreamT[F, X]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit val TreeFunctor: Functor[Tree] = new Functor[Tree] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

  implicit def FailProjectionFunctor[X]: Functor[({type λ[α] = FailProjection[α, X]})#λ] =
    new Functor[({type λ[α] = FailProjection[α, X]})#λ] {
      def fmap[A, B](f: A => B) =
        r => (r.validation match {
          case Success(a) => Success[B, X](a)
          case Failure(e) => Failure[B, X](f(e))
        }).fail
    }

  implicit def ValidationFunctor[X]: Functor[({type λ[α] = Validation[X, α]})#λ] = new Functor[({type λ[α] = Validation[X, α]})#λ] {
    def fmap[A, B](f: A => B) =
      _ map f
  }

}

trait FunctorsLow {
  implicit def TraversableFunctor[CC[X] <: collection.TraversableLike[X, CC[X]] : CanBuildAnySelf]: Functor[CC] = new Functor[CC] {
    def fmap[A, B](f: A => B) =
      r => {
        implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[A, B]
        r map f
      }
  }
}