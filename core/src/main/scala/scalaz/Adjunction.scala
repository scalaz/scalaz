package scalaz

/**
 * An adjunction formed by two functors `F` and `G` such that `F` is left-adjoint to `G`.
 * The composite functor GF is a monad and the composite functor FG is a comonad.
 *
 * The minimal defition is either (unit, counit) or (leftAdjunct, rightAdjunct)
 */
abstract class Adjunction[F[_], G[_]](implicit val F: Functor[F], val G: Functor[G]) { self =>

  /** Puts a value into the monad. */
  def unit[A](a: => A): G[F[A]] = leftAdjunct(a)(x => x)

  /** Extracts a value out of the comonad. */
  def counit[A](a: F[G[A]]): A = rightAdjunct(a)(x => x)

  /** Every `F`-algebra maps to a `G`-coalgebra. */
  def leftAdjunct[A, B](a: => A)(f: F[A] => B): G[B] = G.map(unit(a))(f)

  /** Every `G`-coalgebra maps to an `F`-algebra. */
  def rightAdjunct[A, B](a: F[A])(f: A => G[B]): B = counit(F.map(a)(f))

  /** Adjoint functors annihilate each other. */
  implicit val zapFG: Zap[F, G] = new Zap[F, G] {
    def zapWith[A, B, C](a: F[A], b: G[B])(f: (A, B) => C): C =
      f.tupled(counit(F.map(F.strengthR(a, b))(p => G.strengthL(p._1, p._2))))
  }

  /** Adjoint functors annihilate each other. */
  implicit val zapGF: Zap[G, F] = new Zap[G, F] {
    def zapWith[A, B, C](a: G[A], b: F[B])(f: (A, B) => C): C =
      f.tupled(counit(F.map(F.strengthL(a, b))(p => G.strengthR(p._1, p._2))))
  }

  /** Every adjunction is representable. */
  implicit val representable: Representable[G, F[Unit]] = new Representable[G, F[Unit]] {
    def rep[A](f: F[Unit] => A): G[A] = leftAdjunct(())(f)
    def unrep[A](g: G[A]): F[Unit] => A = fu => rightAdjunct(fu)(u => g)
  }

  /** Every adjunction gives rise to a monad. */
  implicit val monad: Monad[λ[α => G[F[α]]]] = new Monad[λ[α => G[F[α]]]] {
    def point[A](a: => A) = unit(a)
    def bind[A,B](a: G[F[A]])(f: A => G[F[B]]) = G.map(a)(rightAdjunct(_)(f))
  }

  /** Every adjunction gives rise to a comonad. */
  implicit val comonad: Comonad[λ[α => F[G[α]]]] = new Comonad[λ[α => F[G[α]]]] {
    def copoint[A](a: F[G[A]]) = counit(a)
    def cobind[A,B](a: F[G[A]])(f: F[G[A]] => B): F[G[B]] = F.map(a)(leftAdjunct(_)(f))
    def map[A,B](a: F[G[A]])(f: A => B) = cobind(a)(x => f(counit(x)))
    override def cojoin[A](a: F[G[A]]) = cobind(a)(x => x)
  }

  import Adjunction.-|

  /**
   * Adjunctions compose in a natural fashion. If `F -| G` is an adjunction, and `P -| Q` is an
   * adjunction, then PF -| GQ is an adjunction. In fact, adjunctions in Scala form a monoid.
   */
  def compose[P[_], Q[_]](implicit A: P -| Q): λ[α => P[F[α]]] -| λ[α => G[Q[α]]] = {
    implicit val P = A.F
    implicit val Q = A.G
    implicit val PF = P compose F
    implicit val GQ = G compose Q
    new (λ[α => P[F[α]]] -| λ[α => G[Q[α]]]) {
      override def unit[A](a: => A): G[Q[P[F[A]]]] = self.G.map(self.unit(a))(x => A.unit(x))
      override def counit[A](a: P[F[G[Q[A]]]]): A = A.counit(P.map(a)(self.counit))
    }
  }
}

object Adjunction extends AdjunctionInstances {
  type -|[F[_], G[_]] = Adjunction[F, G]

  def apply[F[_], G[_]](implicit A: F -| G, F: Functor[F], G: Functor[F]): F -| G = A
}

sealed abstract class AdjunctionInstances {
  import Adjunction.-|

  implicit def compositeAdjunction[F[_], P[_], G[_], Q[_]](implicit A1: F -| G, A2: P -| Q): λ[α => P[F[α]]] -| λ[α => G[Q[α]]] =
    A1 compose A2

  import Id._
  import std.tuple._
  import std.function._

  implicit def curryUncurryAdjunction[S]: (S, ?) -| (S => ?) =
    new Adjunction[(S, ?), (S => ?)] {
      override def leftAdjunct[A, B](a: => A)(f: ((S, A)) => B): S => B = s => f(s, a)
      override def rightAdjunct[A, B](a: (S, A))(f: A => S => B): B = f(a._2)(a._1)
    }

  implicit val identityAdjunction: Id -| Id =
    new Adjunction[Id, Id] {
      override def leftAdjunct[A, B](a: => A)(f: A => B): B = f(a)
      override def rightAdjunct[A, B](a: A)(f: A => B): B = f(a)
    }

  implicit val f0Adjunction: Function0 -| Function0 =
    new Adjunction[Function0, Function0] {
      override def leftAdjunct[A, B](a: => A)(f: (() => A) => B): () => B = () => f(() => a)
      override def rightAdjunct[A, B](a: () => A)(f: A => () => B): B = f(a())()
    }

  implicit val idF0Adjunction: Id -| Function0 =
    new Adjunction[Id, Function0] {
      override def leftAdjunct[A, B](a: => A)(f: A => B): () => B = () => f(a)
      override def rightAdjunct[A, B](a: A)(f: A => () => B): B = f(a)()
    }

  implicit val f0IdAdjunction: Function0 -| Id =
    new Adjunction[Function0, Id] {
      override def leftAdjunct[A, B](a: => A)(f: (() => A) => B): B = f(() => a)
      override def rightAdjunct[A, B](a: () => A)(f: A => B): B = f(a())
    }

  implicit def writerReaderAdjunction[E]: Adjunction[Writer[E, ?], Reader[E, ?]] =
    new Adjunction[Writer[E, ?], Reader[E, ?]] {
      override def leftAdjunct[A, B](a: => A)(f: Writer[E, A] => B): Reader[E, B] =
        Reader(e => f(Writer(e, a)))
      override def rightAdjunct[A, B](w: Writer[E, A])(f: A => Reader[E, B]): B = {
        val (e, a) = w.run
        f(a)(e)
      }
    }
}

