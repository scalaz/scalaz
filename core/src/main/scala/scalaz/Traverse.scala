package scalaz

trait Traverse[T[_]] extends Functor[T] {
  def traverse[F[_] : Applicative, A, B](f: A => F[B], t: T[A]): F[T[B]]

  import Scalaz._

  override def fmap[A, B](k: T[A], f: A => B) = traverse[Identity, A, B](f(_), k)
}

object Traverse {
  import Scalaz._

  implicit def IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: Identity[A]) = f(t.value) ∘ (b => (b: B))
  }

  implicit def NonEmptyListTraverse: Traverse[NonEmptyList] = new Traverse[NonEmptyList] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: NonEmptyList[A]) = (as.list ↦ f) ∘ ((x: List[B]) => nel(x.head, x.tail))
  }

  implicit def TraversableTraverse[CC[X] <: collection.SeqLike[X, CC[X]] : CanBuildAnySelf]: Traverse[CC] = new Traverse[CC] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: CC[A]): F[CC[B]] = {
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[B, B]
      val ap: Apply[F] = implicitly[Apply[F]]

      // Build up the result using streams to avoid potentially expensive prepend operation on other collections.
      val flistbs: F[Stream[B]] = as.toStream.foldr(Stream.empty[B].η[F])((x, ys) => ap(f(x) ∘ ((a: B) => (b: Stream[B]) => a #:: b), ys))
      flistbs ∘ (_.toList.map(identity)(collection.breakOut))
    }
  }

  implicit def Tuple1Traverse: Traverse[Tuple1] = new Traverse[Tuple1] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: Tuple1[A]) = f(t._1) ∘ (Tuple1(_: B))
  }

  implicit def Tuple2Traverse[X]: Traverse[({type λ[α]=(X, α)})#λ] = new Traverse[({type λ[α]=(X, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: (X, A)): F[(X, B)] =
      f(as._2) ∘ ((b: B) => (as._1, b))
  }

  implicit def Function0Traverse: Traverse[Function0] = new Traverse[Function0] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: () => A) = f(t.apply) ∘ ((b: B) => () => b)
  }

  implicit def OptionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], ta: Option[A]): F[Option[B]] =
      ta match {
        case None => (none[B]) η
        case Some(x) => f(x) ∘ (Some(_: B))
      }
  }

  import concurrent.Promise

  implicit def PromiseTraverse: Traverse[Promise] = new Traverse[Promise] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], ta: Promise[A]): F[Promise[B]] =
      f(ta.get) ∘ (promise(_: B)(ta.strategy))
  }

  implicit def ZipperTraverse: Traverse[Zipper] = new Traverse[Zipper] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], za: Zipper[A]): F[Zipper[B]] = {
      val z = (zipper(_: Stream[B], _: B, _: Stream[B])).curried
      val a = implicitly[Applicative[F]]
      a.apply(a.apply(a.fmap(a.fmap(TraversableTraverse[Stream].traverse[F, A, B](f, za.lefts.reverse), (_: Stream[B]).reverse),
        z), f(za.focus)), TraversableTraverse[Stream].traverse[F, A, B](f, za.rights))
    }
  }

  implicit def ZipStreamTraverse: Traverse[ZipStream] = new Traverse[ZipStream] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], za: ZipStream[A]): F[ZipStream[B]] =
      TraversableTraverse[Stream].traverse[F, A, B](f, za.value) ∘ ((_: Stream[B]) ʐ)
  }

  implicit def TreeTraverse: Traverse[Tree] = new Traverse[Tree] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], ta: Tree[A]): F[Tree[B]] = {
      val trav = (t: Tree[A]) => traverse[F, A, B](f, t)
      val cons = (x: B) => (xs: Stream[Tree[B]]) => node(x, xs)
      val a = implicitly[Applicative[F]]
      a.apply(a.fmap(f(ta.rootLabel), cons), TraversableTraverse[Stream].traverse[F, Tree[A], Tree[B]](trav, ta.subForest))
    }
  }

  implicit def EitherLeftTraverse[X]: Traverse[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Traverse[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: Either.LeftProjection[A, X]): F[Either.LeftProjection[B, X]] =
      as.e match {
        case Right(x) => (Right(x).left: Either.LeftProjection[B, X]) η
        case Left(x) => f(x) ∘ (Left(_: B).left)
      }
  }

  implicit def EitherRightTraverse[X]: Traverse[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Traverse[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: Either.RightProjection[X, A]): F[Either.RightProjection[X, B]] =
      as.e match {
        case Left(x) => (Left(x).right: Either.RightProjection[X, B]) η
        case Right(x) => f(x) ∘ (Right(_: B).right)
      }
  }

  implicit def ValidationTraverse[X]: Traverse[({type λ[α]=Validation[X, α]})#λ] = new Traverse[({type λ[α]=Validation[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: Validation[X, A]): F[Validation[X, B]] = as match {
      case Success(x) => f(x) ∘ (Success(_: B))
      case Failure(x) => (Failure(x): Validation[X, B]) η
    }
  }

  implicit def ValidationFailureTraverse[X]: Traverse[({type λ[α]=FailProjection[α, X]})#λ] = new Traverse[({type λ[α]=FailProjection[α, X]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: FailProjection[A, X]): F[FailProjection[B, X]] =
      as.validation match {
        case Success(x) => (Success(x).fail: FailProjection[B, X]) η
        case Failure(x) => f(x) ∘ (Failure(_: B).fail)
      }
  }

  import java.util.concurrent.Callable

  implicit def CallableTraverse: Traverse[Callable] = new Traverse[Callable] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: Callable[A]): F[Callable[B]] = f(as.call) ∘ (b =>
      new Callable[B] {
        def call = b
      })
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryTraverse[X]: Traverse[({type λ[α]=Entry[X, α]})#λ] = new Traverse[({type λ[α]=Entry[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: Entry[X, A]): F[Entry[X, B]] = f(as.getValue) ∘ ((b: B) => new SimpleImmutableEntry(as.getKey, b))
  }
}
