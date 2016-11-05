package scalaz

////
import scalaz.Id.Id

/**
 * Idiomatic traversal of a structure, as described in
 * [[http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essence of the Iterator Pattern]].
 *
 * @see [[scalaz.Traverse.TraverseLaw]]
 */
////
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  ////

  /** Transform `fa` using `f`, collecting all the `G`s with `ap`. */
  def traverseImpl[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]]

  // derived functions

  /**The composition of Traverses `F` and `G`, `[x]F[G[x]]`, is a Traverse */
  def compose[G[_]](implicit G0: Traverse[G]): Traverse[λ[α => F[G[α]]]] =
    new CompositionTraverse[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /** The composition of Traverse `F` and Bitraverse `G`, `[x, y]F[G[x, y]]`, is a Bitraverse */
  def bicompose[G[_, _]: Bitraverse]: Bitraverse[λ[(α, β) => F[G[α, β]]]] =
    new CompositionTraverseBitraverse[F, G] {
      def F = self
      def G = implicitly
    }

  /**The product of Traverses `F` and `G`, `[x](F[x], G[x]])`, is a Traverse */
  def product[G[_]](implicit G0: Traverse[G]): Traverse[λ[α => (F[α], G[α])]] =
    new ProductTraverse[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Traverse `F` and Traverse1 `G`, `[x](F[x], G[x]])`, is a Traverse1 */
  def product0[G[_]](implicit G0: Traverse1[G]): Traverse1[λ[α => (F[α], G[α])]] =
    new ProductTraverse1R[F, G] {
      def F = self
      def G = G0
    }

  class Traversal[G[_]](implicit G: Applicative[G]) {
    def run[A,B](fa: F[A])(f: A => G[B]): G[F[B]] = traverseImpl[G,A,B](fa)(f)
  }

  // reduce - given monoid
  def traversal[G[_]:Applicative]: Traversal[G] =
    new Traversal[G]
  def traversalS[S]: Traversal[State[S, ?]] =
    new Traversal[State[S, ?]]()(StateT.stateMonad) {
      override def run[A, B](fa: F[A])(f: A => State[S, B]) = traverseS(fa)(f)
    }

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    traversal[G].run(fa)(f)

  /** A version of `traverse` that infers the type constructor `G`. */
  final def traverseU[A, GB](fa: F[A])(f: A => GB)(implicit G: Unapply[Applicative, GB]): G.M[F[G.A]] /*G[F[B]]*/ =
    G.TC.traverse(fa)(G.leibniz.onF(f))(this)

  /** A version of `traverse` where a subsequent monadic join is applied to the inner result. */
  final def traverseM[A, G[_], B](fa: F[A])(f: A => G[F[B]])(implicit G: Applicative[G], F: Bind[F]): G[F[B]] =
    G.map(G.traverse(fa)(f)(this))(F.join)

  /** Traverse with `State`. */
  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] =
    traverseSTrampoline[S, Id.Id, A, B](fa)(f)

  def runTraverseS[S,A,B](fa: F[A], s: S)(f: A => State[S,B]): (S, F[B]) =
    traverseS(fa)(f)(s)

  /** Traverse `fa` with a `State[S, G[B]]`, internally using a `Trampoline` to avoid stack overflow. */
  def traverseSTrampoline[S, G[_] : Applicative, A, B](fa: F[A])(f: A => State[S, G[B]]): State[S, G[F[B]]] = {
    import Free._
    implicit val A = StateT.stateTMonadState[S, Trampoline].compose(Applicative[G])
    State[S, G[F[B]]](s => {
      val st = traverse[λ[α => StateT[Trampoline, S, G[α]]], A, B](fa)(f(_: A).lift[Trampoline])
      st.run(s).run
    })
  }

  /** Traverse `fa` with a `Kleisli[G, S, B]`, internally using a `Trampoline` to avoid stack overflow. */
  def traverseKTrampoline[S, G[_] : Applicative, A, B](fa: F[A])(f: A => Kleisli[G, S, B]): Kleisli[G, S, F[B]] = {
    import Free._
    implicit val A = Kleisli.kleisliMonadReader[Trampoline, S].compose(Applicative[G])
    Kleisli[G, S, F[B]](s => {
      val kl = traverse[λ[α => Kleisli[Trampoline, S, G[α]]], A, B](fa)(z => Kleisli[Id, S, G[B]](i => f(z)(i)).lift[Trampoline]).run(s)
      kl.run
    })
  }

  /** Traverse with the identity function. */
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
    traversal[G].run[G[A], A](fga)(ga => ga)

  /** Traverse with `State`. */
  def sequenceS[S,A](fga: F[State[S,A]]): State[S,F[A]] =
    traverseS(fga)(x => x)

  /** A version of `sequence` that infers the nested type constructor. */
  final def sequenceU[A](self: F[A])(implicit G: Unapply[Applicative, A]): G.M[F[G.A]] /*G[F[A]] */ =
    G.TC.traverse(self)(x => G.apply(x))(this)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traversal[Id](Id.id).run(fa)(f)

  def foldLShape[A,B](fa: F[A], z: B)(f: (B,A) => B): (B, F[Unit]) =
    runTraverseS(fa, z)(a => State.modify(f(_, a)))

  override def foldLeft[A,B](fa: F[A], z: B)(f: (B,A) => B): B = foldLShape(fa, z)(f)._1

  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B = foldLShape(fa, F.zero)((b, a) => F.append(b, f(a)))._1

  override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B) =
    foldMap(fa)((a: A) => (Endo.endo(f(a, _: B)))) apply z

  def reverse[A](fa: F[A]): F[A] = {
    val (as, shape) = mapAccumL(fa, scala.List[A]())((t,h) => (h :: t,h))
    runTraverseS(shape, as)(_ => for {
      e <- State.get
      _ <- State.put(e.tail)
    } yield e.head)._2
  }

  def zipWith[A,B,C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): (List[B], F[C]) =
    runTraverseS(fa, toList(fb))(a => for {
      bs <- State.get
      _ <- State.put(if (bs.isEmpty) bs else bs.tail)
    } yield f(a, bs.headOption))

  def zipWithL[A,B,C](fa: F[A], fb: F[B])(f: (A,Option[B]) => C): F[C] = zipWith(fa, fb)(f)._2
  def zipWithR[A,B,C](fa: F[A], fb: F[B])(f: (Option[A],B) => C): F[C] = zipWith(fb, fa)((b,oa) => f(oa,b))._2

  def indexed[A](fa: F[A]): F[(Int, A)] = mapAccumL(fa, 0) { case (s, a) => (s + 1, (s, a)) }._2

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] = zipWithL(fa, fb)((_,_))
  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] = zipWithR(fa, fb)((_,_))

  def mapAccumL[S,A,B](fa: F[A], z: S)(f: (S,A) => (S,B)): (S, F[B]) =
    runTraverseS(fa, z)(a => for {
      s1 <- State.init[S]
      (s2,b) = f(s1,a)
      _ <- State.put(s2)
    } yield b)

  def mapAccumR[S,A,B](fa: F[A], z: S)(f: (S,A) => (S,B)): (S, F[B]) =
    mapAccumL(reverse(fa), z)(f) match { case (s, fb) => (s, reverse(fb)) }

  trait TraverseLaw extends FunctorLaw {
    /** Traversal through the [[scalaz.Id]] effect is equivalent to `Functor#map` */
    def identityTraverse[A, B](fa: F[A], f: A => B)(implicit FB: Equal[F[B]]) = {
      FB.equal(traverse[Id, A, B](fa)(f), map(fa)(f))
    }

    /** Two sequentially dependent effects can be fused into one, their composition */
    def sequentialFusion[N[_], M[_], A, B, C](fa: F[A], amb: A => M[B], bnc: B => N[C])
                                               (implicit N: Applicative[N], M: Applicative[M], MN: Equal[M[N[F[C]]]]): Boolean = {
      type MN[A] = M[N[A]]
      val t1: MN[F[C]] = M.map(traverse[M, A, B](fa)(amb))(fb => traverse[N, B, C](fb)(bnc))
      val t2: MN[F[C]] = traverse[MN, A, C](fa)(a => M.map(amb(a))(bnc))(M compose N)
      MN.equal(t1, t2)
    }

    /** Traversal with the `point` function is the same as applying the `point` function directly */
    def purity[G[_], A](fa: F[A])(implicit G: Applicative[G], GFA: Equal[G[F[A]]]): Boolean =
      GFA.equal(traverse[G, A, A](fa)(G.point[A](_)), G.point(fa))

    /**
     * @param nat A natural transformation from `M` to `N` for which these properties hold:
     *            `(a: A) => nat(Applicative[M].point[A](a)) === Applicative[N].point[A](a)`
     *            `(f: M[A => B], ma: M[A]) => nat(Applicative[M].ap(ma)(f)) === Applicative[N].ap(nat(ma))(nat(f))`
     */
    def naturality[N[_], M[_], A](nat: (M ~> N))
                                 (fma: F[M[A]])
                                 (implicit N: Applicative[N], M: Applicative[M], NFA: Equal[N[F[A]]]): Boolean = {
      val n1: N[F[A]] = nat[F[A]](sequence[M, A](fma))
      val n2: N[F[A]] = sequence[N, A](map(fma)(ma => nat(ma)))
      NFA.equal(n1, n2)
    }

    /** Two independent effects can be fused into a single effect, their product. */
    def parallelFusion[N[_], M[_], A, B](fa: F[A], amb: A => M[B], anb: A => N[B])
                                        (implicit N: Applicative[N], M: Applicative[M], MN: Equal[(M[F[B]], N[F[B]])]): Boolean = {
      type MN[A] = (M[A], N[A])
      val t1: MN[F[B]] = (traverse[M, A, B](fa)(amb), traverse[N, A, B](fa)(anb))
      val t2: MN[F[B]] = traverse[MN, A, B](fa)(a => (amb(a), anb(a)))(M product N)
      MN.equal(t1, t2)
    }
  }
  def traverseLaw = new TraverseLaw {}

  ////
  val traverseSyntax = new scalaz.syntax.TraverseSyntax[F] { def F = Traverse.this }
}

object Traverse {
  @inline def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  ////

  ////
}
