package scalaz

import Scalaz._
import Liskov._

trait Recursion extends
Nu_s with
Mu_s with
Nus with
Mus with
Cofree_s with
Cofrees with
CofreeRec_s with
CofreeRecs

/**
 * Possibly negative corecursion
 */
trait Nu_[F[_]] extends Immutable {
  def out: F[Nu_[F]]
}

object Nu_ extends Nu_s {
  def apply[F[_]](v: => F[Nu_[F]]): Nu_[F] = new Nu_[F] {
    def out = v
  }
}

trait Nu_s {
  def unapply[F[_]](v: Nu_[F]) = Some(v.out)

  implicit def unwrapNu_[F[_]](v: Nu_[F]) = v.out
}

/**
 * Possibly negative recursion.
 * Note Mu_ is not a subtype of Nu_, because generating Nu_[F] from Mu_[F]
 * requires a Functor for F or subtyping.
 */
trait Mu_[F[_]] extends Immutable {
  val out: F[Mu_[F]]
}

object Mu_ extends Mu_s {
  def apply[F[_]](v: F[Mu_[F]]): Mu_[F] = new Mu_[F] {
    val out = v
  }
}

trait Mu_s {
  def unapply[F[_]](v: Mu_[F]) = Some(v.out)

  implicit def unwrapMu_[F[_]](v: Mu_[F]) = v.out
}

/**Positive corecursion */

// ideal: trait Nu[+F[+_]] extends Nu_[F] {
// works: trait Nu[+F[+_]] {
trait Nu[F[+ _]] extends Nu_[F] {
  def out: F[Nu[F]]
}

object Nu extends Nus {
  def apply[F[+ _]](v: => F[Nu[F]]): Nu[F] = new Nu[F] {
    def out = v
  }
}

trait Nus {
  def unapply[F[+ _]](v: Nu[F]) = Some(v.out)

  implicit def rewrapNu[F[+ _], G[+ _]](v: Nu[F])(
      implicit lt: F[Nu[F]] <~< G[Nu[F]]
      ): Nu[G] = v.asInstanceOf[Nu[G]]

  implicit def unwrapNu[F[_]](v: Mu_[F]) = v.out
}

/**Positive recursion */

// ideal: trait Mu[+F[+_]] extends Nu[F] with Mu_[F] {
// works: trait Mu[+F[+_]] extends Nu[F] {
trait Mu[F[+ _]] extends Nu[F] with Mu_[F] {
  val out: F[Mu[F]]
}

object Mu extends Mus {
  def apply[F[+ _]](v: F[Mu[F]]): Mu[F] = new Mu[F] {
    val out = v
  }
}

trait Mus {
  def unapply[F[+ _]](v: Mu[F]) = Some(v.out)

  implicit def rewrapMu[F[+ _], G[+ _]](v: Mu[F])(
      implicit lt: F[Mu[F]] <~< G[Mu[F]]
      ): Mu[G] = v.asInstanceOf[Mu[G]]

  implicit def unwrapMu[F[+ _]](v: Mu[F]) = v.out
}

/**Cofree corecursion */
trait Cofree_[F[_], A] extends Immutable {
  val extract: A

  def out: F[Cofree_[F, A]]

  def scanr[B](g: (A, F[Cofree_[F, B]]) => B)(implicit f: Functor[F]): Cofree_[F, B] = {
    lazy val qs = out map (_.scanr(g))
    Cofree_[F, B](g(extract, qs), qs)
  }
}

object Cofree_ extends Cofree_s {
  def apply[F[_], A](
                        a: A,
                        v: => F[Cofree_[F, A]]
                        ): Cofree_[F, A] =
    new Cofree_[F, A] {
      val extract = a

      def out = v
    }
}

trait Cofree_s {
  def unapply[F[_], A](v: Cofree_[F, A]) = Some((v.extract, v.out))

  implicit def unwrapCofree_[F[_], A](v: Cofree_[F, A]) = (v.extract, v.out)

  implicit def Cofree_Traverse[T[+ _] : Traverse]
  : Traverse[({type λ[α] = Cofree_[T, α]})#λ] =
    new Traverse[({type λ[α] = Cofree_[T, α]})#λ] {
      def traverse[F[_] : Applicative, A, B](f: A => F[B]): Cofree_[T, A] => F[Cofree_[T, B]] = t => {
        implicit val af = implicitly[Applicative[F]].applicFunctor
        (f(t.extract) <**> t.out.traverse(traverse(f) apply (_)))((a, o) => Cofree_(a, o))
      }
    }

}

/**Positive cofree corecursion */
// ideal: trait Cofree[+F[+_],+A] extends Nu[F] with Cofree_[F,A] {
// works: trait Cofree[+F[+_],+A] extends Nu[F] {
trait Cofree[F[+ _], A] extends Nu[F] with Cofree_[F, A] {
  val extract: A

  def out: F[Cofree[F, A]]

  def scanr[B](g: (A, F[Cofree[F, B]]) => B)(implicit f: Functor[F]): Cofree[F, B] = {
    lazy val qs = out map (_.scanr(g))
    Cofree[B, F](g(extract, qs), qs)
  }
}

object Cofree extends Cofrees {
  def apply[A, F[+ _]](
                          a: A,
                          v: => F[Cofree[F, A]]
                          ): Cofree[F, A] =
    new Cofree[F, A] {
      val extract = a

      def out = v
    }
}

trait Cofrees {
  def unapply[F[+ _], A](v: Cofree[F, A]) = Some((v.extract, v.out))

  implicit def rewrapCofree[F[+ _], G[+ _], A, B >: A](v: Cofree[F, A])(
      implicit lt: F[Cofree[F, A]] <~< G[Cofree[F, A]]
      ): Cofree[G, B] = v.asInstanceOf[Cofree[G, B]]

  implicit def unwrapCofree[F[+ _], A](v: Cofree[F, A]) = (v.extract, v.out)

  implicit def CofreeTraverse[T[+ _] : Traverse]
  : Traverse[({type λ[α] = Cofree[T, α]})#λ] =
    new Traverse[({type λ[α] = Cofree[T, α]})#λ] {
      def traverse[F[_] : Applicative, A, B](f: A => F[B]): Cofree[T, A] => F[Cofree[T, B]] = t => {
        implicit val af = implicitly[Applicative[F]].applicFunctor
        (f(t.extract) <**> t.out.traverse(traverse(f) apply (_)))((a, o) => Cofree(a, o))
      }
    }

}

/**Cofree recursion */
trait CofreeRec_[F[_], A] {
  val extract: A
  val out: F[CofreeRec_[F, A]]
}

object CofreeRec_ extends CofreeRec_s {
  def apply[F[_], A](
                        a: A,
                        v: => F[CofreeRec_[F, A]]
                        ): CofreeRec_[F, A] =
    new CofreeRec_[F, A] {
      val extract = a
      val out = v
    }
}

trait CofreeRec_s {
  def unapply[F[_], A](v: CofreeRec_[F, A]) = Some((v.extract, v.out))

  implicit def unwrapCofreeRec_[F[_], A](v: CofreeRec_[F, A]) = (v.extract, v.out)

  implicit def CofreeRec_Traverse[T[+ _] : Traverse]
  : Traverse[({type λ[α] = CofreeRec_[T, α]})#λ] =
    new Traverse[({type λ[α] = CofreeRec_[T, α]})#λ] {
      def traverse[F[_] : Applicative, A, B](f: A => F[B]): CofreeRec_[T, A] => F[CofreeRec_[T, B]] = t => {
        implicit val af = implicitly[Applicative[F]].applicFunctor
        (f(t.extract) <**> t.out.traverse(traverse(f) apply (_)))((a, o) => CofreeRec_(a, o))
      }
    }
}

/**Positive cofree recursion */

// ideal: trait CofreeRec[+F[+_],A] extends Mu[F] with CofreeRec_[F,A] {
// works: trait CofreeRec[+F[+_],A] extends Mu[F] {
trait CofreeRec[F[+ _], A] extends Mu[F] with CofreeRec_[F, A] {
  val extract: A
  val out: F[CofreeRec[F, A]]
}

object CofreeRec extends CofreeRecs {
  def apply[F[+ _], A](
                          a: A,
                          v: => F[CofreeRec[F, A]]
                          ): CofreeRec[F, A] =
    new CofreeRec[F, A] {
      val extract = a
      val out = v
    }
}

trait CofreeRecs {
  def unapply[F[+ _], A](v: CofreeRec[F, A]) = Some((v.extract, v.out))

  implicit def rewrapCofreeRec[F[+ _], G[+ _], A, B >: A](v: CofreeRec[F, A])(
      implicit lt: F[CofreeRec[F, A]] <~< G[CofreeRec[F, A]]
      ): CofreeRec[G, B] = v.asInstanceOf[CofreeRec[G, A]]

  implicit def unwrapCofreeRec[F[+ _], A](v: CofreeRec[F, A]) = (v.extract, v.out)

  implicit def CofreeRecTraverse[T[+ _] : Traverse]
  : Traverse[({type λ[α] = CofreeRec[T, α]})#λ] =
    new Traverse[({type λ[α] = CofreeRec[T, α]})#λ] {
      def traverse[F[_] : Applicative, A, B](f: A => F[B]): CofreeRec[T, A] => F[CofreeRec[T, B]] = t => {
        implicit val af = implicitly[Applicative[F]].applicFunctor
        (f(t.extract) <**> t.out.traverse(traverse(f) apply (_)))((a, o) => CofreeRec(a, o))
      }
    }

}
