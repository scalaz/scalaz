package scalaz

////
import scalaz.Id.Id

/**
 * A [[scalaz.Traverse]] where `traverse` is total over
 * [[scalaz.Apply]]s.  That is, `toList` cannot return an empty list.
 */
////
trait Traverse1[F[_]] extends Traverse[F] with Foldable1[F] { self =>
  ////

  /**The product of Traverse1 `F` and `G`, `[x](F[x], G[x]])`, is a Traverse1 */
  def product[G[_]](implicit G0: Traverse1[G]): Traverse1[({type λ[α] = (F[α], G[α])})#λ] = new ProductTraverse1[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /** Transform `fa` using `f`, collecting all the `G`s with `ap`. */
  def traverse1Impl[G[_]:Apply,A,B](fa: F[A])(f: A => G[B]): G[F[B]]

  // derived functions
  override def traverseImpl[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    traverse1Impl(fa)(f)

  override def foldMap1[A,B](fa: F[A])(f: A => B)(implicit F: Semigroup[B]): B =
    foldLeft1(traverse1Impl[Id, A, B](fa)(a => f(a)))(F.append(_, _))

  def traverse1[G[_], A, B](fa: F[A])(f: A => G[B])(implicit a: Apply[G]): G[F[B]] =
    traverse1Impl(fa)(f)

  def sequence1[G[_]:Apply,A](fga: F[G[A]]): G[F[A]] =
    traverse1Impl[G, G[A], A](fga)(identity)

  trait Traverse1Law extends TraverseLaw {
    /** Traversal through the [[scalaz.Id]] effect is equivalent to
      * `Functor#map`.
      */
    def identityTraverse1[A, B](fa: F[A], f: A => B)(implicit FB: Equal[F[B]]) = {
      FB.equal(traverse1[Id, A, B](fa)(f), map(fa)(f))
    }

    /** Two sequentially dependent effects can be fused into one,
      * their composition.
      */
    def sequentialFusion1[N[_], M[_], A, B, C](fa: F[A], amb: A => M[B], bnc: B => N[C])
                                               (implicit N: Apply[N], M: Apply[M], MN: Equal[M[N[F[C]]]]): Boolean = {
      type MN[A] = M[N[A]]
      val t1: MN[F[C]] = M.map(traverse1[M, A, B](fa)(amb))(fb => traverse1[N, B, C](fb)(bnc))
      val t2: MN[F[C]] = traverse1[MN, A, C](fa)(a => M.map(amb(a))(b => bnc(b)))(M compose N)
      MN.equal(t1, t2)
    }

    /**
     * `naturality` specialized to `sequence1`.
     */
    def naturality1[N[_], M[_], A](nat: (M ~> N))
                                 (fma: F[M[A]])
                                 (implicit N: Apply[N], M: Apply[M], NFA: Equal[N[F[A]]]): Boolean = {
      val n1: N[F[A]] = nat[F[A]](sequence1[M, A](fma))
      val n2: N[F[A]] = sequence1[N, A](map(fma)(ma => nat(ma)))
      NFA.equal(n1, n2)
    }

    /** Two independent effects can be fused into a single effect, their product. */
    def parallelFusion1[N[_], M[_], A, B](fa: F[A], amb: A => M[B], anb: A => N[B])
                                        (implicit N: Apply[N], M: Apply[M], MN: Equal[(M[F[B]], N[F[B]])]): Boolean = {
      type MN[A] = (M[A], N[A])
      val t1: MN[F[B]] = (traverse1[M, A, B](fa)(amb), traverse1[N, A, B](fa)(anb))
      val t2: MN[F[B]] = traverse1[MN, A, B](fa)(a => (amb(a), anb(a)))(M product N)
      MN.equal(t1, t2)
    }
  }
  def traverse1Law = new Traverse1Law {}

  ////
  val traverse1Syntax = new scalaz.syntax.Traverse1Syntax[F] { def F = Traverse1.this }
}

object Traverse1 {
  @inline def apply[F[_]](implicit F: Traverse1[F]): Traverse1[F] = F

  ////

  ////
}
