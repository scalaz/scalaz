package scalaz

import Identity._

trait Traverse[T[_]] {
  def traverse[F[_] : Applicative, A, B](f: A => F[B]): T[A] => F[T[B]]

  def functor: Functor[T] = new Functor[T] {
    def fmap[A, B](f: A => B) = t => {
      val k = traverse[Identity, A, B](a => id(f(a)))
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