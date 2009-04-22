package scalaz

trait Traverse[T[_]] extends Functor[T] {
  def traverse[F[_], A, B](f: A => F[B], t: T[A])(implicit a: Applicative[F]): F[T[B]]

  def fmap[A, B](k: T[A], f: A => B) = traverse[Identity, A, B](a => Identity.id(f(a)), k).value
}

object Traverse {
  import S._
  import MA._

  implicit val IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_], A, B](f: A => F[B], t: Identity[A])(implicit a: Applicative[F]) = a.fmap(f(t.value), Identity.id(_: B))
  }

  implicit val NonEmptyListTraverse: Traverse[NonEmptyList] = new Traverse[NonEmptyList] {
    def traverse[F[_], A, B](f: A => F[B], as: NonEmptyList[A])(implicit a: Applicative[F]) = a.fmap(ListTraverse.traverse[F, A, B](f, as.list), (x: List[B]) => NonEmptyList.nel(x.head, x.tail))
  }

  implicit val Tuple1Traverse: Traverse[Tuple1] = new Traverse[Tuple1] {
    def traverse[F[_], A, B](f: A => F[B], t: Tuple1[A])(implicit a: Applicative[F]) = a.fmap(f(t._1), Tuple1(_: B))
  }

  implicit val Function0Traverse: Traverse[Function0] = new Traverse[Function0] {
    def traverse[F[_], A, B](f: A => F[B], t: Function0[A])(implicit a: Applicative[F]) = a.fmap(f(t.apply), (b: B) => () => b)
  }

  implicit val ListTraverse: Traverse[List] = new Traverse[List] {
    def traverse[F[_], A, B](f: A => F[B], as: List[A])(implicit a: Applicative[F]): F[List[B]] =
      as.foldRight[F[List[B]]](a.pure(Nil))((x, ys) => a(a.fmap(f(x), (a: B) => (b: List[B]) => a :: b), ys))
  }

  implicit val StreamTraverse: Traverse[Stream] = new Traverse[Stream] {
    def traverse[F[_], A, B](f: A => F[B], as: Stream[A])(implicit a: Applicative[F]): F[Stream[B]] = as.foldr[F[Stream[B]]](a.pure(Stream.empty), (x, ys) => a(a.fmap(f(x), (a: B) => (b: Stream[B]) => Stream.cons(a, b)), ys))
  }

  implicit val OptionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[F[_], A, B](f: A => F[B], ta: Option[A])(implicit a: Applicative[F]): F[Option[B]] =
      ta match {
        case None => a.pure(None)
        case Some(x) => a.fmap(f(x), (Some(_: B)))
      }
  }

  import Zipper.zipper

  implicit val ZipperTraverse: Traverse[Zipper] = new Traverse[Zipper] {
    def traverse[F[_], A, B](f: A => F[B], za: Zipper[A])(implicit a: Applicative[F]): F[Zipper[B]] = {
      val z = (zipper(_: Stream[B], _: B, _: Stream[B])).curry
      a.apply(a.apply(a.fmap(a.fmap(StreamTraverse.traverse[F, A, B](f, za.lefts.reverse), (_:Stream[B]).reverse),
        z), f(za.focus)), StreamTraverse.traverse[F, A, B](f, za.rights))
    }
  }

  implicit val ZipStreamTraverse: Traverse[ZipStream] = new Traverse[ZipStream] {
    def traverse[F[_], A, B](f: A => F[B], za: ZipStream[A])(implicit a: Applicative[F]): F[ZipStream[B]] =
      a.fmap(StreamTraverse.traverse[F, A, B](f, za.value), (_: Stream[B]) |!|)
  }

  implicit val ArrayTraverse: Traverse[Array] = new Traverse[Array] {
    def traverse[F[_], A, B](f: A => F[B], as: Array[A])(implicit a: Applicative[F]): F[Array[B]] =
      a.fmap(ListTraverse.traverse[F, A, B](f, as.toList), ((_: List[B]).toArray))
  }

  implicit def EitherLeftTraverse[X]: Traverse[PartialApply1Of2[Either.LeftProjection, X]#Flip] = new Traverse[PartialApply1Of2[Either.LeftProjection, X]#Flip] {
    def traverse[F[_], A, B](f: A => F[B], as: Either.LeftProjection[A, X])(implicit a: Applicative[F]): F[Either.LeftProjection[B, X]] =
      as.e match {
        case Right(x) => a.pure(Right(x).left)
        case Left(x) => a.fmap(f(x), (Left(_: B).left))
      }
  }

  implicit def EitherRightTraverse[X]: Traverse[PartialApply1Of2[Either.RightProjection, X]#Apply] = new Traverse[PartialApply1Of2[Either.RightProjection, X]#Apply] {
    def traverse[F[_], A, B](f: A => F[B], as: Either.RightProjection[X, A])(implicit a: Applicative[F]): F[Either.RightProjection[X, B]] =
      as.e match {
        case Left(x) => a.pure(Left(x).right)
        case Right(x) => a.fmap(f(x), (Right(_: B).right))
      }
  }
}
