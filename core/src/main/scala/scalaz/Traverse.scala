package scalaz

trait Traverse[T[_]] extends Functor[T] {
  def traverse[F[_]: Applicative, A, B](f: A => F[B], t: T[A]): F[T[B]]

  import Scalaz._

  override def fmap[A, B](k: T[A], f: A => B) = traverse[Identity, A, B](f(_), k)
}

object Traverse {
  import Scalaz._

  implicit def IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], t: Identity[A]) = f(t.value) ∘ (b => (b: B))
  }

  implicit def NonEmptyListTraverse: Traverse[NonEmptyList] = new Traverse[NonEmptyList] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: NonEmptyList[A]) = (as.list ↦ f) ∘ ((x: List[B]) => nel(x.head, x.tail))
  }

  implicit def Tuple1Traverse: Traverse[Tuple1] = new Traverse[Tuple1] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], t: Tuple1[A]) = f(t._1) ∘ (Tuple1(_: B))
  }

  implicit def Tuple2Traverse[X]: Traverse[PartialApply1Of2[Tuple2, X]#Apply] = new Traverse[PartialApply1Of2[Tuple2, X]#Apply] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Tuple2[X, A]): F[Tuple2[X, B]] =
      f(as._2) ∘ ((b: B) => (as._1, b))
  }

  implicit def Function0Traverse: Traverse[Function0] = new Traverse[Function0] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], t: Function0[A]) = f(t.apply) ∘ ((b: B) => () => b)
  }

  implicit def ListTraverse: Traverse[List] = new Traverse[List] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: List[A]): F[List[B]] = {
      val a = implicitly[Apply[F]]
      as.reverse.foldLeft((Nil: List[B]) η)((ys, x) => a(f(x) ∘ ((a: B) => (b: List[B]) => a :: b), ys))
    }
  }

  implicit def StreamTraverse: Traverse[Stream] = new Traverse[Stream] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Stream[A]): F[Stream[B]] =
      as.foldr[F[Stream[B]]]((Stream.Empty: Stream[B]) η, (x, ys) => implicitly[Apply[F]].apply(f(x) ∘ ((a: B) => (b: Stream[B]) => a #:: b), ys))
  }

  implicit def OptionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], ta: Option[A]): F[Option[B]] =
      ta match {
        case None => (None: Option[B]) η 
        case Some(x) => f(x) ∘ (Some(_: B))
      }
  }

  import concurrent.Promise
  implicit def PromiseTraverse: Traverse[Promise] = new Traverse[Promise] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], ta: Promise[A]): F[Promise[B]] =
      f(ta.get) ∘ (promise(_: B)(ta.strategy))
  }

  implicit def ZipperTraverse: Traverse[Zipper] = new Traverse[Zipper] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], za: Zipper[A]): F[Zipper[B]] = {
      val z = (zipper(_: Stream[B], _: B, _: Stream[B])).curry
      val a = implicitly[Applicative[F]]
      a.apply(a.apply(a.fmap(a.fmap(StreamTraverse.traverse[F, A, B](f, za.lefts.reverse), (_: Stream[B]).reverse),
        z), f(za.focus)), StreamTraverse.traverse[F, A, B](f, za.rights))
    }
  }

  implicit def ZipStreamTraverse: Traverse[ZipStream] = new Traverse[ZipStream] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], za: ZipStream[A]): F[ZipStream[B]] =
      StreamTraverse.traverse[F, A, B](f, za.value) ∘ ((_: Stream[B]) ʐ)
  }

  implicit def TreeTraverse: Traverse[Tree] = new Traverse[Tree] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], ta: Tree[A]): F[Tree[B]] = {
      val trav = (t: Tree[A]) => traverse[F, A, B](f, t)
      val cons = (x: B) => (xs: Stream[Tree[B]]) => node(x, xs)
      val a = implicitly[Applicative[F]]
      a.apply(a.fmap(f(ta.rootLabel), cons), StreamTraverse.traverse[F, Tree[A], Tree[B]](trav, ta.subForest))
    }
  }

  implicit def GenericArrayTraverse: Traverse[GArray] = new Traverse[GArray] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: GArray[A]): F[GArray[B]] =
      as.toList ↦ f ∘ ((x: List[B]) => collection.mutable.GenericArray(x: _*))
  }

  implicit def EitherLeftTraverse[X]: Traverse[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Traverse[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Either.LeftProjection[A, X]): F[Either.LeftProjection[B, X]] =
      as.e match {
        case Right(x) => (Right(x).left: Either.LeftProjection[B, X]) η
        case Left(x) => f(x) ∘ (Left(_: B).left)
      }
  }

  implicit def EitherRightTraverse[X]: Traverse[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Traverse[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Either.RightProjection[X, A]): F[Either.RightProjection[X, B]] =
      as.e match {
        case Left(x) => (Left(x).right: Either.RightProjection[X, B]) η
        case Right(x) => f(x) ∘ (Right(_: B).right)
      }
  }

  implicit def ValidationTraverse[X]: Traverse[PartialApply1Of2[Validation, X]#Apply] = new Traverse[PartialApply1Of2[Validation, X]#Apply] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Validation[X, A]): F[Validation[X, B]] = as match {
      case Success(x) => f(x) ∘ (Success(_: B))
      case Failure(x) => (Failure(x): Validation[X, B]) η
    }
  }

  implicit def ValidationFailureTraverse[X]: Traverse[PartialApply1Of2[FailProjection, X]#Flip] = new Traverse[PartialApply1Of2[FailProjection, X]#Flip] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: FailProjection[A, X]): F[FailProjection[B, X]] =
      as.validation match {
        case Success(x) => (Success(x).fail: FailProjection[B, X]) η
        case Failure(x) => f(x) ∘ (Failure(_: B).fail)
      }
  }

  import java.util.concurrent.Callable

  implicit def CallableTraverse: Traverse[Callable] = new Traverse[Callable] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Callable[A]): F[Callable[B]] =  f(as.call) ∘ (b => new Callable[B] { def call = b })
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryTraverse[X]: Traverse[PartialApply1Of2[Entry, X]#Apply] = new Traverse[PartialApply1Of2[Entry, X]#Apply] {
    def traverse[F[_]: Applicative, A, B](f: A => F[B], as: Entry[X, A]): F[Entry[X, B]] = f(as.getValue) ∘ ((b: B) => new SimpleImmutableEntry(as.getKey, b))
  }
}
