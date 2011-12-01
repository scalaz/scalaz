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

trait ContFunctions {
  def runCont[R,A](cont:Cont[R,A])(f:A => R):R = cont(f)
  def const[R,A](g: => A):Cont[R,A] = new Cont[R,A] {
    def apply(k:A => R):R = k(g)
  }
  def cont[R,A](g:(A => R) => R):Cont[R,A] = new Cont[R,A] {
    def apply(k:A => R):R = g(k)
  }

  def callCC[R,A,B](k:(A => Cont[R,B]) => Cont[R,A]):Cont[R,A] =
    cont((c:A=>R) => runCont(k((a:A) => cont((_:B => R) => c(a))))(c))
}

object Cont extends ContFunctions {
  @inline def apply[R,A](a: => A):Cont[R,A] = const(a)
}

private[scalaz] trait ContFunctor[R] extends Functor[({type λ[α] = Cont[R, α]})#λ] {
  override def map[A, B](fa: Cont[R, A])(f: A => B): Cont[R, B] = fa map f
}

private[scalaz] trait ContPointed[R] extends Pointed[({type λ[α] = Cont[R, α]})#λ] with ContFunctor[R] {
  def point[A](a: => A): Cont[R, A] = Cont(a)
}

private[scalaz] trait ContMonad[R] extends Monad[({type λ[α] = Cont[R, α]})#λ] with ContPointed[R] {
  def bind[A, B](fa: Cont[R, A])(f: A => Cont[R, B]): Cont[R, B] = fa flatMap f
}

// private[scalaz] trait OptionTMonadTrans extends MonadTrans[OptionT] {
//   def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): OptionT[G, A] =
//     OptionT[G, A](G.map[A, Option[A]](a)((a: A) => Some(a)))

//   def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = OptionT[M, x]})#f ~> ({type f[x] = OptionT[N, x]})#f) {
//     def apply[A](fa: OptionT[M, A]): OptionT[N, A] = OptionT(f.apply(fa.runT))
//   }
// }
