package scalaz

trait Traverse[T[_]] {
  def traverse[F[_] : Applicative, A, B](f: A => F[B]): T[A] => F[T[B]]

  def functor: Functor[T] = new Functor[T] {
    def fmap[A, B](f: A => B) = t => {
      val k = traverse[Identity, A, B](a => Identity.id(f(a)))
      k(t).value
    }
  }

  def fmap[A, B](f: A => B): T[A] => T[B] =
    functor.fmap(f)

  def deriving[G[_]](implicit n: ^**^[G, T]): Traverse[G] =
    new Traverse[G] {
      def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
        a => {
          val tr = Traverse.this.traverse(f)
          implicitly[Applicative[F]].fmap((z: T[B]) => n.pack(z))(tr(n.unpack(a)))
        }
    }

}

object Traverse extends Traverses

trait Traverses extends TraversesLow {

  implicit def IdentityTraverse: Traverse[Identity] = new Traverse[Identity] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      t => implicitly[Applicative[F]].fmap((b: B) => Identity.id(b))(f(t.value))
  }

  implicit def Tuple1Traverse: Traverse[Tuple1] = new Traverse[Tuple1] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      t => implicitly[Applicative[F]].fmap(Tuple1(_: B))(f(t._1))
  }

  implicit def Tuple2Traverse[T]: Traverse[({type λ[α]=(T, α)})#λ] = new Traverse[({type λ[α]=(T, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case (t, u) => implicitly[Applicative[F]].fmap((t, _: B))(f(u))
    }
  }

  implicit def Tuple3Traverse[T, U]: Traverse[({type λ[α]=(T, U, α)})#λ] = new Traverse[({type λ[α]=(T, U, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case (t, u, v) => implicitly[Applicative[F]].fmap((t, u, _: B))(f(v))
    }
  }

  implicit def Tuple4Traverse[T, U, V]: Traverse[({type λ[α]=(T, U, V, α)})#λ] = new Traverse[({type λ[α]=(T, U, V, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case (t, u, v, w) => implicitly[Applicative[F]].fmap((t, u, v, _: B))(f(w))
    }
  }

  implicit def Tuple5Traverse[T, U, V, W]: Traverse[({type λ[α]=(T, U, V, W, α)})#λ] = new Traverse[({type λ[α]=(T, U, V, W, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case (t, u, v, w, x) => implicitly[Applicative[F]].fmap((t, u, v, w, _: B))(f(x))
    }
  }

  implicit def Tuple6Traverse[T, U, V, W, X]: Traverse[({type λ[α]=(T, U, V, W, X, α)})#λ] = new Traverse[({type λ[α]=(T, U, V, W, X, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case (t, u, v, w, x, y) => implicitly[Applicative[F]].fmap((t, u, v, w, x, _: B))(f(y))
    }
  }

  implicit def Tuple7Traverse[T, U, V, W, X, Y]: Traverse[({type λ[α]=(T, U, V, W, X, Y, α)})#λ] = new Traverse[({type λ[α]=(T, U, V, W, X, Y, α)})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case (t, u, v, w, x, y, z) => implicitly[Applicative[F]].fmap((t, u, v, w, x, y, _: B))(f(z))
    }
  }

  implicit def Function0Traverse: Traverse[Function0] = new Traverse[Function0] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      t => implicitly[Applicative[F]].fmap(() => (_: B))(f(t.apply))
  }

  implicit def NonEmptyListTraverse: Traverse[NonEmptyList] = new Traverse[NonEmptyList] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      as => implicitly[Applicative[F]].fmap((x: List[B]) => NonEmptyList.nel(x.head, x.tail))(implicitly[Traverse[List]].traverse(f).apply(as.list))
  }

  implicit def StreamTraverse: Traverse[Stream] = new Traverse[Stream] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      implicitly[Foldr[Stream]].foldr[A, F[Stream[B]]](x => ys =>
        implicitly[Applicative[F]].apply(implicitly[Applicative[F]].fmap((a: B) => (b: Stream[B]) => a #:: b)(f(x)))(ys))(implicitly[Applicative[F]].point(Stream.Empty))

  }

  implicit def ListTraverse: Traverse[List] = new Traverse[List] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      _.reverse.foldLeft(implicitly[Applicative[F]].point(Nil: List[B]))((ys, x) =>
        implicitly[Applicative[F]].apply(implicitly[Applicative[F]].fmap((a: B) => (b: List[B]) => a :: b)(f(x)))(ys))
  }

  implicit def OptionTraverse: Traverse[Option] = new Traverse[Option] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case None => implicitly[Applicative[F]].point(None: Option[B])
      case Some(x) => implicitly[Applicative[F]].fmap((b: B) => Some(b): Option[B])(f(x))
    }
  }

  implicit val TreeTraverse: Traverse[Tree] = new Traverse[Tree] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      ta => {
        val a = implicitly[Applicative[F]]
        a.apply(a.fmap((x: B) => (xs: Stream[Tree[B]]) => Tree.node(x, xs))(f(ta.rootLabel)))(implicitly[Traverse[Stream]].traverse[F, Tree[A], Tree[B]](traverse[F, A, B](f).apply(_: Tree[A])).apply(ta.subForest))
      }
  }

  implicit def ZipperTraverse: Traverse[Zipper] = new Traverse[Zipper] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      za => {
        val z = (Zipper.zipper(_: Stream[B], _: B, _: Stream[B])).curried
        val a = implicitly[Applicative[F]]
        a.apply(a.apply(a.fmap((s: Stream[B]) => z(s.reverse))(implicitly[Traverse[Stream]].traverse[F, A, B](f) apply (za.lefts.reverse)))(f(za.focus)))(implicitly[Traverse[Stream]].traverse[F, A, B](f) apply (za.rights))
      }
  }

  implicit def EitherLeftTraverse[X]: Traverse[({type λ[α]=Either.LeftProjection[α, X]})#λ] = new Traverse[({type λ[α]=Either.LeftProjection[α, X]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      _.e match {
        case Right(x) => implicitly[Applicative[F]].point(Right(x).left: Either.LeftProjection[B, X])
        case Left(x) => implicitly[Applicative[F]].fmap((b: B) => (Left(b): Either[B, X]).left)(f(x))
      }
  }

  implicit def EitherRightTraverse[X]: Traverse[({type λ[α]=Either.RightProjection[X, α]})#λ] = new Traverse[({type λ[α]=Either.RightProjection[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      _.e match {
        case Left(x) => implicitly[Applicative[F]].point(Left(x).right: Either.RightProjection[X, B])
        case Right(x) => implicitly[Applicative[F]].fmap((b: B) => (Right(b): Either[X, B]).right)(f(x))
      }
  }

  implicit def EitherTraverse[X]: Traverse[({type λ[α]=Either[X, α]})#λ] = new Traverse[({type λ[α]=Either[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) = {
      case Left(x) => implicitly[Applicative[F]].point(Left(x): Either[X, B])
      case Right(x) => implicitly[Applicative[F]].fmap((b: B) => (Right(b): Either[X, B]))(f(x))
    }
  }

  implicit def ValidationFailureTraverse[X]: Traverse[({type λ[α]=FailProjection[α, X]})#λ] = new Traverse[({type λ[α]=FailProjection[α, X]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      _.validation.fold(
        x => implicitly[Applicative[F]].fmap(Validation.failure[X](_: B).fail)(f(x))
      , x => implicitly[Applicative[F]].point(Validation.success(x).fail)
      )
  }

  implicit def ValidationTraverse[X]: Traverse[({type λ[α]=Validation[X, α]})#λ] = new Traverse[({type λ[α]=Validation[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      _.fold(
        x => implicitly[Applicative[F]].point(Validation.failure[B](x))
      , x => implicitly[Applicative[F]].fmap((b: B) => (Validation.success[X](b)))(f(x))
      )
  }

  import java.util.concurrent.Callable

  implicit def CallableTraverse: Traverse[Callable] = new Traverse[Callable] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      as => implicitly[Applicative[F]].fmap((b: B) =>
        new Callable[B] {
          def call = b
        })(f(as.call))
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryTraverse[X]: Traverse[({type λ[α]=Entry[X, α]})#λ] = new Traverse[({type λ[α]=Entry[X, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      as => implicitly[Applicative[F]].fmap[B, Entry[X, B]](new SimpleImmutableEntry(as.getKey, _))(f(as.getValue))
  }

}

trait TraversesLow {

  implicit def TraversableTraverse[CC[X] <: collection.SeqLike[X, CC[X]] : CanBuildAnySelf]: Traverse[CC] = new Traverse[CC] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B]) =
      as => {
        implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[B, B]
        val ap = implicitly[Applicative[F]]
        // Build up the result using streams to avoid potentially expensive prepend operation on other collections.
        val flistbs = implicitly[Foldr[Stream]].foldr[A, F[Stream[B]]](x => ys =>
          implicitly[Applicative[F]].apply(ap.fmap(((a: B) => (b: Stream[B]) => a #:: b))(f(x)))(ys))(ap.point(Stream.empty[B]))(as.toStream)
        ap.fmap((xs: Stream[B]) => {
          val builder = cbf.apply()
          for (x <- xs) builder += x
          builder.result
        })(flistbs)
      }

  }
}