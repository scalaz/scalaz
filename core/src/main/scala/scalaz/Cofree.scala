package scalaz

/** A cofree comonad for some functor `S`, i.e. an `S`-branching stream. */
case class Cofree[S[_], A](head: A, tail: S[Cofree[S, A]])(implicit S: Functor[S]) {

  /** Alias for `head`, for compatibility with Scalaz 6 */
  final def extract: A = head

  /** Alias for `head`, for compatibility with Scalaz 6 */
  final def copure: A = head

  /** Alias for `tail`, for compatibility with Scalaz 6 */
  final def out: S[Cofree[S, A]] = tail

  final def map[B](f: A => B): Cofree[S, B] =
    applyCofree(f, _ map f)

  /** Alias for `extend` */
  final def =>>[B](f: Cofree[S, A] => B): Cofree[S, B] = this extend f

  /** Redecorates this structure with a computation whose context is the entire structure under that value. */
  final def extend[B](f: Cofree[S, A] => B): Cofree[S, B] =
    applyTail(f(this), _ extend f)

  /** Folds over this cofree structure, returning all the intermediate values in a new structure. */
  def scanr[B](g: (A, S[Cofree[S, B]]) => B): Cofree[S, B] = {
    lazy val qs = S.map(tail)(_ scanr g)
    Cofree(g(head, qs), qs)
  }

  /** Redecorates the structure with values representing entire substructures. */
  final def duplicate: Cofree[S, Cofree[S, A]] =
    applyTail(this, _.duplicate)

  /** Returns the components of this structure in a tuple. */
  final def toPair: (A, S[Cofree[S, A]]) = (head, tail)

  /** Changes the branching functor by the given natural transformation. */
  final def mapBranching[T[_]:Functor](f: S ~> T): Cofree[T, A] =
    Cofree(head, f(S.map(tail)(_ mapBranching f)))

  /** Modifies the first branching with the given natural transformation. */
  final def mapFirstBranching(f: S ~> S): Cofree[S, A] =
    Cofree(head, f(tail))

  /** Injects a constant value into this structure. */
  final def inject[B](b: B): Cofree[S, B] =
    applyTail(b, _ inject b)

  /** Applies `f` to the head and `g` through the tail. */
  final def applyCofree[B](f: A => B, g: Cofree[S, A] => Cofree[S, B]): Cofree[S, B] =
    Cofree(f(head), S.map(tail)(g))

  /** Replaces the head with `b` and applies `g` through the tail. */
  final def applyTail[B](b: B, g: Cofree[S, A] => Cofree[S, B]): Cofree[S, B] =
    applyCofree(x => b, g)

  /** Applies a function `f` to a value in this comonad and a corresponding value in the dual monad, annihilating both. */
  final def zapWith[G[_], B, C](bs: Free[G, B])(f: (A, B) => C)(implicit G: Functor[G], d: Zap[S, G]): C =
    Zap.comonadMonadZap.zapWith(this, bs)(f)

  /** Applies a function in a monad to the corresponding value in this comonad, annihilating both. */
  final def zap[G[_], B](fs: Free[G, A => B])(implicit G: Functor[G], d: Zap[S, G]): B =
    zapWith(fs)((a, f) => f(a))
}

object Cofree extends CofreeFunctions with CofreeInstances


trait CofreeFunctions {

  /** Cofree corecursion. */
  def unfoldC[F[_], A](a: A)(f: A => F[A])(implicit F: Functor[F]): Cofree[F, A] =
    Cofree(a, F.map(f(a))(unfoldC(_)(f)))

}

trait CofreeInstances {
  implicit def cofreeComonad[S[_]]: Comonad[({type f[x] = Cofree[S, x]})#f] = new CofreeComonad[S] {}
}

private[scalaz] trait CofreeComonad[S[_]] extends Comonad[({type f[x] = Cofree[S, x]})#f] {
  def copoint[A](p: Cofree[S, A]) = p.head

  override def cojoin[A](a: Cofree[S, A]) = a.duplicate

  def map[A, B](fa: Cofree[S, A])(f: A => B) = fa map f

  def cobind[A, B](fa: Cofree[S, A])(f: (Cofree[S, A]) => B) = fa extend f
}
