package scala

import scalaz.Scalaz._
import scalaz.Liskov._
import scalaz.Applicative
import scalaz.Traverse

/**
 * Possibly negative corecursion 
 */
trait Nu_[F[_]] extends Immutable {
  def out: F[Nu_[F]]
}
object Nu_ { 
  def apply[F[_]](v: => F[Nu_[F]]) : Nu_[F] = new Nu_[F] { def out = v }
  def unapply[F[_]](v: Nu_[F]) = Some(v.out)
  implicit def unwrap[F[_]](v: Nu_[F]) = v.out
}

/** 
 * Possibly negative recursion. 
 * Note Mu_ is not a subtype of Nu_, because generating Nu_[F] from Mu_[F] 
 * requires a Functor for F or subtyping.
 */
trait Mu_[F[_]] extends Immutable {
  val out: F[Mu_[F]]
}
object Mu_ { 
  def apply[F[_]](v: F[Mu_[F]]) : Mu_[F] = new Mu_[F] { val out = v }
  def unapply[F[_]](v: Mu_[F]) = Some(v.out)
  implicit def unwrap[F[_]](v: Mu_[F]) = v.out
} 

/** Positive corecursion */

// ideal: trait Nu[+F[+_]] extends Nu_[F] {
// works: trait Nu[+F[+_]] { 
trait Nu[F[+_]] extends Nu_[F] {
  def out: F[Nu[F]]
}
object Nu { 
  def apply[F[+_]](v: => F[Nu[F]]) : Nu[F] = new Nu[F] { def out = v }
  def unapply[F[+_]](v : Nu[F]) = Some(v.out)
  implicit def rewrap[F[+_],G[+_]](v : Nu[F])(
    implicit lt: F[Nu[F]] <~< G[Nu[F]]
  ) : Nu[G] = v.asInstanceOf[Nu[G]]
  implicit def unwrap[F[_]](v: Mu_[F]) = v.out
}

/** Positive recursion */

// ideal: trait Mu[+F[+_]] extends Nu[F] with Mu_[F] {
// works: trait Mu[+F[+_]] extends Nu[F] {
trait Mu[F[+_]] extends Nu[F] with Mu_[F] {
  val out: F[Mu[F]]
}
object Mu {
  def apply[F[+_]](v: F[Mu[F]]) : Mu[F] = new Mu[F] { val out = v } 
  def unapply[F[+_]](v: Mu[F]) = Some(v.out)
  implicit def rewrap[F[+_],G[+_]](v : Mu[F])(
    implicit lt: F[Mu[F]] <~< G[Mu[F]]
  ) : Mu[G] = v.asInstanceOf[Mu[G]]
  implicit def unwrap[F[+_]](v: Mu[F]) = v.out
}

/** Cofree corecursion */
trait Cofree_[F[_],A] extends Immutable {
  val extract: A
  def out: F[Cofree_[F,A]]
}
object Cofree_ {
  def apply[F[_],A](
    a: A,
    v: => F[Cofree_[F,A]]
  ) : Cofree_[F,A] = 
  new Cofree_[F,A] {
    val extract = a
    def out = v
  }
  def unapply[F[_],A](v: Cofree_[F,A]) = Some((v.extract, v.out))
  implicit def unwrap[F[_],A](v: Cofree_[F,A]) = (v.extract, v.out)

  implicit def Cofree_Traverse[T[+_]:Traverse]
    : Traverse[({type λ[α]=Cofree_[T, α]})#λ] =
  new Traverse[({type λ[α]=Cofree_[T, α]})#λ] {
    def traverse[F[_]: Applicative, A, B](f : A => F[B], t : Cofree_[T,A]) : F[Cofree_[T,B]] = { 
       (f(t.extract) <**> t.out.traverse(traverse(f,_)))((a, o) => Cofree_(a,o))
    }
  }
}

/** Positive cofree corecursion */
// ideal: trait Cofree[+F[+_],+A] extends Nu[F] with Cofree_[F,A] {
// works: trait Cofree[+F[+_],+A] extends Nu[F] {
trait Cofree[F[+_],A] extends Nu[F] with Cofree_[F,A] {
  val extract: A 
  def out: F[Cofree[F,A]]
}
object Cofree { 
  def apply[A,F[+_]](
    a: A,
    v: => F[Cofree[F,A]]
  ) : Cofree[F,A] = 
  new Cofree[F,A] {
    val extract = a
    def out = v
  }
  def unapply[F[+_],A](v: Cofree[F,A]) = Some((v.extract, v.out))
  implicit def rewrap[F[+_],G[+_],A,B >: A](v : Cofree[F,A])(
    implicit lt: F[Cofree[F,A]] <~< G[Cofree[F,A]]
  ) : Cofree[G,B] = v.asInstanceOf[Cofree[G,B]]
  implicit def unwrap[F[+_],A](v: Cofree[F,A]) = (v.extract, v.out)

  implicit def CofreeTraverse[T[+_]:Traverse]
    : Traverse[({type λ[α]=Cofree[T, α]})#λ] =
  new Traverse[({type λ[α]=Cofree[T, α]})#λ] {
    def traverse[F[_]: Applicative, A, B](f : A => F[B], t : Cofree[T,A]) : F[Cofree[T,B]] = { 
       (f(t.extract) <**> t.out.traverse(traverse(f,_)))((a, o) => Cofree(a,o))
    }
  }
}

/** Cofree recursion */
trait CofreeRec_[F[_],A] {
  val extract: A
  val out: F[CofreeRec_[F,A]]
}
object CofreeRec_ { 
  def apply[F[_],A](
    a: A,
    v: => F[CofreeRec_[F,A]]
  ) : CofreeRec_[F,A] = 
  new CofreeRec_[F,A] {
    val extract = a
    val out = v
  }
  def unapply[F[_],A](v: CofreeRec_[F,A]) = Some((v.extract, v.out))
  implicit def unwrap[F[_],A](v: CofreeRec_[F,A]) = (v.extract, v.out)

  implicit def CofreeRec_Traverse[T[+_]:Traverse]
    : Traverse[({type λ[α]=CofreeRec_[T, α]})#λ] =
  new Traverse[({type λ[α]=CofreeRec_[T, α]})#λ] {
    def traverse[F[_]: Applicative, A, B](f : A => F[B], t : CofreeRec_[T,A]) : F[CofreeRec_[T,B]] = { 
       (f(t.extract) <**> t.out.traverse(traverse(f,_)))((a, o) => CofreeRec_(a,o))
    }
  }
}

/** Positive cofree recursion */

// ideal: trait CofreeRec[+F[+_],A] extends Mu[F] with CofreeRec_[F,A] {
// works: trait CofreeRec[+F[+_],A] extends Mu[F] {
trait CofreeRec[F[+_],A] extends Mu[F] with CofreeRec_[F,A] {
  val extract: A
  val out: F[CofreeRec[F,A]]
}
object CofreeRec { 
  def apply[F[+_],A](
    a: A,
    v: => F[CofreeRec[F,A]]
  ) : CofreeRec[F,A] = 
  new CofreeRec[F,A] {
    val extract = a
    val out = v
  }
  def unapply[F[+_],A](v: CofreeRec[F,A]) = Some((v.extract, v.out))
  implicit def rewrap[F[+_],G[+_],A,B >: A](v : CofreeRec[F,A])(
    implicit lt: F[CofreeRec[F,A]] <~< G[CofreeRec[F,A]]
  ) : CofreeRec[G,B] = v.asInstanceOf[CofreeRec[G,A]]
  implicit def unwrap[F[+_],A](v: CofreeRec[F,A]) = (v.extract, v.out)

  implicit def CofreeRecTraverse[T[+_]:Traverse]
    : Traverse[({type λ[α]=CofreeRec[T, α]})#λ] =
  new Traverse[({type λ[α]=CofreeRec[T, α]})#λ] {
    def traverse[F[_]: Applicative, A, B](f : A => F[B], t : CofreeRec[T,A]) : F[CofreeRec[T,B]] = { 
       (f(t.extract) <**> t.out.traverse(traverse(f,_)))((a, o) => CofreeRec(a,o))
    }
  }
}
