package scalaz

/** 
 * An adjunction formed by two functors `F` and `G`. The composite functor GF is a monad
 * and the composite functor FG is a comonad.
 *
 * The minimal defition is either (unit, counit) or (leftAdjunct, rightAdjunct)
 */
abstract class Adjunction[F[_], G[_]](implicit val F: Functor[F], val G: Functor[G]) {

  /** Puts a value into the monad. */
  def unit[A](a: A): G[F[A]] = leftAdjunct(a)(x => x)
  
  /** Extracts a value out of the comonad. */
  def counit[A](a: F[G[A]]): A = rightAdjunct(a)(x => x)
  
  /** Every `F`-algebra maps to a `G`-coalgebra. */
  def leftAdjunct[A, B](a: A)(f: F[A] => B): G[B] = G.map(unit(a))(f)
  
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
  implicit val monad: Monad[({type λ[α] = G[F[α]]})#λ] = new Monad[({type λ[α] = G[F[α]]})#λ] {
    def point[A](a: => A) = unit(a)
    def bind[A,B](a: G[F[A]])(f: A => G[F[B]]) = G.map(a)(rightAdjunct(_)(f))
  }

  /** Every adjunction gives rise to a comonad. */
  implicit val comonad: Comonad[({type λ[α] = F[G[α]]})#λ] = new Comonad[({type λ[α] = F[G[α]]})#λ] {
    def copoint[A](a: F[G[A]]) = counit(a)
    def cobind[A,B](a: F[G[A]])(f: F[G[A]] => B): F[G[B]] = F.map(a)(leftAdjunct(_)(f))
    def map[A,B](a: F[G[A]])(f: A => B) = cobind(a)(x => f(counit(x)))
    def cojoin[A](a: F[G[A]]) = cobind(a)(x => x)
  } 
}

object Adjunction extends AdjunctionInstances with AdjunctionFunctions

trait AdjunctionFunctions {
  def apply[F[_]: Functor, G[_]: Functor](eta: Id ~> ({type λ[α] = G[F[α]]})#λ, epsilon: ({type λ[α] = F[G[α]]})#λ ~> Id): Adjunction[F, G] = new Adjunction[F, G] {
    override def unit[A](a: A) = eta(a)
    override def counit[A](a: F[G[A]]) = epsilon(a)
  }
}

trait AdjunctionInstances {
  implicit def productAdjunction[F1[_], G1[_], F2[_], G2[_]](implicit A1: Adjunction[F1, G1], A2: Adjunction[F2, G2]): Adjunction[({type λ[α] = F2[F1[α]]})#λ, ({type λ[α] = G1[G2[α]]})#λ] = {
    implicit val F1 = A1.F
    implicit val F2 = A2.F
    implicit val G1 = A1.G
    implicit val G2 = A2.G
    implicit val F2F1 = F2 compose F1
    implicit val G1G2 = G1 compose G2
    new Adjunction[({type λ[α] = F2[F1[α]]})#λ, ({type λ[α] = G1[G2[α]]})#λ] {
      override def unit[A](a: A) = A1.G.map(A1.unit(a))(A2.unit)
      override def counit[A](a: F2[F1[G1[G2[A]]]]) = A2.counit(A2.F.map(a)(A1.counit))
    }
  }
}
