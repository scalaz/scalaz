package scalaz

////
/**
 *
 */
////
trait Cont[R,A] { self =>
  ////
  def apply(k:A => R):R
  def map[B](f:A => B):Cont[R,B] = new Cont[R,B] {
    def apply(k:B => R):R = self.apply(k compose f)
  }
  def flatMap[B](f:A => Cont[R,B]):Cont[R,B] = new Cont[R,B] {
    def apply(k:B => R):R = self.apply((a:A) => f(a).apply(k))
  }
  ////
}

trait ContInstances2 {
  implicit def contTFunctor[R]: Functor[({type λ[α] = Cont[R, α]})#λ] = new ContFunctor[R] {}
}

trait ContInstances1 extends ContInstances2 {
  implicit def contTPointed[R]: Pointed[({type λ[α] = Cont[R, α]})#λ] = new ContPointed[R] {}
}

trait ContInstances0 extends ContInstances1 {
  implicit def contTMonad[R]: Monad[({type λ[α] = Cont[R, α]})#λ] = new ContMonad[R] {}
}

trait ContInstances extends ContInstances0

trait ContFunctions extends ContInstances {
  def runCont[R,A](cont:Cont[R,A])(f:A => R):R = cont(f)
  def const[R,A](g: => A):Cont[R,A] = new Cont[R,A] {
    def apply(k:A => R):R = k(g)
  }
  def cont[R,A](g:(A => R) => R):Cont[R,A] = new Cont[R,A] {
    def apply(k:A => R):R = g(k)
  }
  def exitCC[R,A](r: => R):Cont[R,A] = cont(_ => r)

  def callCC[R,A,B](k:(A => Cont[R,B]) => Cont[R,A]):Cont[R,A] =
    cont((c:A=>R) => runCont(k((a:A) => cont((_:B => R) => c(a))))(c))

  implicit def ContToKleisli[F[_],R,A](f:ContT[F,R,A]):Kleisli[F,A=>F[R],R] = Kleisli[F,A=>F[R],R](k => f(k))
}

object Cont extends ContFunctions {
  @inline def apply[R,A](g:(A => R) => R):Cont[R,A] = cont(g)
}

private[scalaz] trait ContFunctor[R] extends Functor[({type λ[α] = Cont[R, α]})#λ] {
  override def map[A, B](fa: Cont[R, A])(f: A => B): Cont[R, B] = fa map f
}

private[scalaz] trait ContPointed[R] extends Pointed[({type λ[α] = Cont[R, α]})#λ] with ContFunctor[R] {
  def point[A](a: => A): Cont[R, A] = Cont.const(a)
}

private[scalaz] trait ContMonad[R] extends Monad[({type λ[α] = Cont[R, α]})#λ] with ContPointed[R] {
  def bind[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] = fa flatMap f
}