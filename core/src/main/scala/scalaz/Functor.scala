package scalaz

/**
 * Covariant function application in an environment. i.e. a covariant Functor.
 *
 * <p>
 * All functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == fmap(a, identity)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g. fmap(a, f compose g) == fmap(fmap(g, a), f)</code></li>
 * </ol>
 * </p>
 */
trait Functor[F[_]] extends InvariantFunctor[F] {
  def fmap[A, B](r: F[A], f: A => B): F[B]

  final def xmap[A, B](ma: F[A], f: A => B, g: B => A) = fmap(ma, f)
}

object Functor {
  import Scalaz._

  implicit def IdentityFunctor: Functor[Identity] = new Functor[Identity] {
    def fmap[A, B](r: Identity[A], f: A => B) = f(r.value)
  }

  implicit def TraversableFunctor[CC[X] <: collection.TraversableLike[X, CC[X]] : CanBuildAnySelf]: Functor[CC] = new Functor[CC] {
    def fmap[A, B](r: CC[A], f: A => B) = {
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[A, B]
      r map f
    }
  }

  implicit def NonEmptyListFunctor = new Functor[NonEmptyList] {
    def fmap[A, B](r: NonEmptyList[A], f: A => B) = r map f
  }

  implicit def ConstFunctor[BB: Monoid] = new Functor[({type λ[α]=Const[BB, α]})#λ] {
    def fmap[A, B](r: Const[BB, A], f: (A) => B) = Const(r.value)
  }

  implicit def StateFunctor[S] = new Functor[({type λ[α]=State[S, α]})#λ] {
    def fmap[A, B](r: State[S, A], f: A => B) = r map f
  }

  implicit def ZipStreamFunctor: Functor[ZipStream] = new Functor[ZipStream] {
    def fmap[A, B](r: ZipStream[A], f: A => B) = r.value map f ʐ
  }

  implicit def Tuple1Functor: Functor[Tuple1] = new Functor[Tuple1] {
    def fmap[A, B](r: Tuple1[A], f: A => B) = Tuple1(f(r._1))
  }

  implicit def Tuple2Functor[R]: Functor[({type λ[α]=(R, α)})#λ] = new Functor[({type λ[α]=(R, α)})#λ] {
    def fmap[A, B](r: (R, A), f: A => B) = (r._1, f(r._2))
  }

  implicit def Tuple3Functor[R, S]: Functor[({type λ[α]=(R, S, α)})#λ] = new Functor[({type λ[α]=(R, S, α)})#λ] {
    def fmap[A, B](r: (R, S, A), f: A => B) = (r._1, r._2, f(r._3))
  }

  implicit def Tuple4Functor[R, S, T]: Functor[({type λ[α]=(R, S, T, α)})#λ] = new Functor[({type λ[α]=(R, S, T, α)})#λ] {
    def fmap[A, B](r: (R, S, T, A), f: A => B) = (r._1, r._2, r._3, f(r._4))
  }

  implicit def Tuple5Functor[R, S, T, U]: Functor[({type λ[α]=(R, S, T, U, α)})#λ] = new Functor[({type λ[α]=(R, S, T, U, α)})#λ] {
    def fmap[A, B](r: (R, S, T, U, A), f: A => B) = (r._1, r._2, r._3, r._4, f(r._5))
  }

  implicit def Tuple6Functor[R, S, T, U, V]: Functor[({type λ[α]=(R, S, T, U, V, α)})#λ] = new Functor[({type λ[α]=(R, S, T, U, V, α)})#λ] {
    def fmap[A, B](r: (R, S, T, U, V, A), f: A => B) = (r._1, r._2, r._3, r._4, r._5, f(r._6))
  }

  implicit def Tuple7Functor[R, S, T, U, V, W]: Functor[({type λ[α]=(R, S, T, U, V, W, α)})#λ] = new Functor[({type λ[α]=(R, S, T, U, V, W, α)})#λ] {
    def fmap[A, B](r: (R, S, T, U, V, W, A), f: A => B) = (r._1, r._2, r._3, r._4, r._5, r._6, f(r._7))
  }

  implicit def Function0Functor: Functor[Function0] = new Functor[Function0] {
    def fmap[A, B](r: Function0[A], f: A => B) = new Function0[B] {
      def apply = f(r.apply)
    }
  }

  implicit def Function1Functor[R]: Functor[({type λ[α]=(R) => α})#λ] = new Functor[({type λ[α]=(R) => α})#λ] {
    def fmap[A, B](r: R => A, f: A => B) = r andThen f
  }

  implicit def Function2Functor[R, S]: Functor[({type λ[α]=(R, S) => α})#λ] = new Functor[({type λ[α]=(R, S) => α})#λ] {
    def fmap[A, B](r: (R, S) => A, f: A => B) = (t1: R, t2: S) => f(r(t1, t2))
  }

  implicit def Function3Functor[R, S, T]: Functor[({type λ[α]=(R, S, T) => α})#λ] = new Functor[({type λ[α]=(R, S, T) => α})#λ] {
    def fmap[A, B](r: (R, S, T) => A, f: A => B) = (t1: R, t2: S, t3: T) => f(r(t1, t2, t3))
  }

  implicit def Function4Functor[R, S, T, U]: Functor[({type λ[α]=(R, S, T, U) => α})#λ] = new Functor[({type λ[α]=(R, S, T, U) => α})#λ] {
    def fmap[A, B](r: (R, S, T, U) => A, f: A => B) = (t1: R, t2: S, t3: T, t4: U) => f(r(t1, t2, t3, t4))
  }

  implicit def Function5Functor[R, S, T, U, V]: Functor[({type λ[α]=(R, S, T, U, V) => α})#λ] = new Functor[({type λ[α]=(R, S, T, U, V) => α})#λ] {
    def fmap[A, B](r: (R, S, T, U, V) => A, f: A => B) = (t1: R, t2: S, t3: T, t4: U, t5: V) => f(r(t1, t2, t3, t4, t5))
  }

  implicit def Function6Functor[R, S, T, U, V, W]: Functor[({type λ[α]=(R, S, T, U, V, W) => α})#λ] = new Functor[({type λ[α]=(R, S, T, U, V, W) => α})#λ] {
    def fmap[A, B](r: (R, S, T, U, V, W) => A, f: A => B) = (t1: R, t2: S, t3: T, t4: U, t5: V, t6: W) => f(r(t1, t2, t3, t4, t5, t6))
  }

  implicit def OptionFunctor: Functor[Option] = new Functor[Option] {
    def fmap[A, B](r: Option[A], f: A => B) = r map f
  }

  implicit def FirstOptionFunctor: Functor[FirstOption] = new Functor[FirstOption] {
    def fmap[A, B](r: FirstOption[A], f: A => B) = (r.value map f).fst
  }

  implicit def LastOptionFunctor: Functor[LastOption] = new Functor[LastOption] {
    def fmap[A, B](r: LastOption[A], f: A => B) = (r.value map f).lst
  }

  implicit def EitherLeftFunctor[X]: Functor[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Functor[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def fmap[A, B](r: Either.LeftProjection[A, X], f: A => B) = r.map(f).left
  }

  implicit def EitherRightFunctor[X]: Functor[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Functor[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def fmap[A, B](r: Either.RightProjection[X, A], f: A => B) = r.map(f).right
  }

  implicit def ResponderFunctor: Functor[Responder] = new Functor[Responder] {
    def fmap[A, B](r: Responder[A], f: A => B) = r map f
  }

  implicit def IterVFunctor[X]: Functor[({type λ[α]=IterV[X, α]})#λ] = new Functor[({type λ[α]=IterV[X, α]})#λ] {
    import IterV._
    def fmap[A, B](r: IterV[X, A], f: A => B) = {
      r fold (
              done = (a, i) => Done(f(a), i),
              cont = k => Cont(i => fmap(k(i), f))
              )
    }
  }

  implicit def KleisliFunctor[M[_], P](implicit ff: Functor[M]): Functor[({type λ[α]=Kleisli[M, P, α]})#λ] = new Functor[({type λ[α]=Kleisli[M, P, α]})#λ] {
    def fmap[A, B](k: Kleisli[M, P, A], f: A => B): Kleisli[M, P, B] = ☆((p: P) => ff.fmap(k(p), f))
  }

  import java.util.concurrent.Callable

  implicit def CallableFunctor: Functor[Callable] = new Functor[Callable] {
    def fmap[A, B](r: Callable[A], f: A => B) = new Callable[B] {
      def call = f(r.call)
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryFunctor[X]: Functor[({type λ[α]=Entry[X, α]})#λ] = new Functor[({type λ[α]=Entry[X, α]})#λ] {
    def fmap[A, B](r: Entry[X, A], f: A => B) = new SimpleImmutableEntry(r.getKey, f(r.getValue))
  }

  implicit def ValidationFunctor[X]: Functor[({type λ[α]=Validation[X, α]})#λ] = new Functor[({type λ[α]=Validation[X, α]})#λ] {
    def fmap[A, B](r: Validation[X, A], f: A => B) = r match {
      case Success(a) => Success(f(a))
      case Failure(e) => Failure(e)
    }
  }

  implicit def ValidationFailureFunctor[X]: Functor[({type λ[α]=FailProjection[α, X]})#λ] = new Functor[({type λ[α]=FailProjection[α, X]})#λ] {
    def fmap[A, B](r: FailProjection[A, X], f: A => B) = (r.validation match {
      case Success(a) => Success(a)
      case Failure(e) => Failure(f(e))
    }).fail
  }

  implicit def ZipperFunctor: Functor[Zipper] = new Functor[Zipper] {
    def fmap[A, B](z: Zipper[A], f: A => B) = zipper(z.lefts map f, f(z.focus), z.rights map f)
  }

  implicit def TreeFunctor: Functor[Tree] = new Functor[Tree] {
    def fmap[A, B](t: Tree[A], f: A => B): Tree[B] = node(f(t.rootLabel), t.subForest.map(fmap(_: Tree[A], f)))
  }

  implicit def TreeLocFunctor: Functor[TreeLoc] = new Functor[TreeLoc] {
    def fmap[A, B](t: TreeLoc[A], f: A => B): TreeLoc[B] = {
      val ff = (_: Tree[A]).map(f)
      loc(t.tree map f, t.lefts map ff, t.rights map ff,
        t.parents.map((ltr) => (ltr._1 map ff, f(ltr._2), ltr._3 map ff)))
    }
  }

  import FingerTree._

  implicit def ViewLFunctor[S[_]](implicit s: Functor[S]): Functor[({type λ[α]=ViewL[S, α]})#λ] = new Functor[({type λ[α]=ViewL[S, α]})#λ] {
    def fmap[A, B](t: ViewL[S, A], f: A => B): ViewL[S, B] =
      t.fold(EmptyL[S, B], (x, xs) => f(x) &: s.fmap(xs, f))
  }

  implicit def ViewRFunctor[S[_]](implicit s: Functor[S]): Functor[({type λ[α]=ViewR[S, α]})#λ] = new Functor[({type λ[α]=ViewR[S, α]})#λ] {
    def fmap[A, B](t: ViewR[S, A], f: A => B): ViewR[S, B] =
      t.fold(EmptyR[S, B], (xs, x) => s.fmap(xs, f) :& f(x))
  }

  import scalaz.concurrent.Promise
  implicit def PromiseFunctor: Functor[Promise] = new Functor[Promise] {
    def fmap[A, B](t: Promise[A], f: A => B): Promise[B] = {
      t.bind(a => promise(f(a))(t.strategy))
    }
  }

  // todo use this rather than all the specific java.util._ Functor instances once the scala bug is fixed.
  // http://lampsvn.epfl.ch/trac/scala/ticket/2782
  /*implicit*/
  def JavaCollectionFunctor[S[X] <: java.util.Collection[X] : Empty]: Functor[S] = new Functor[S] {
    def fmap[A, B](r: S[A], f: A => B) = {
      val a: S[B] = <∅>
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
}
