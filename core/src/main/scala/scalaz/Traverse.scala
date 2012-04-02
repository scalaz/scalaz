package scalaz

////
/**
 * Idiomatic traversal of a structure, as described in
 * [[http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf The Essense of the Iterator Pattern]].
 *
 * @see [[scalaz.Traverse.TraverseLaw]]
 */
////
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  ////
  def traverseImpl[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]]

  class Traversal[G[_]](implicit G: Applicative[G]) { 
    def run[A,B](fa: F[A])(f: A => G[B]): G[F[B]] = traverseImpl[G,A,B](fa)(f)
  }

  // reduce - given monoid
  def traversal[G[_]:Applicative]: Traversal[G] = 
    new Traversal[G]
  def traversalS[S]: Traversal[({type f[x]=State[S,x]})#f] = 
    new Traversal[({type f[x]=State[S,x]})#f]()(StateT.stateMonad)

  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] = 
    traversal[G].run(fa)(f)
  def traverseS[S,A,B](fa: F[A])(f: A => State[S,B]): State[S,F[B]] = 
    traversalS[S].run(fa)(f)
  def runTraverseS[S,A,B](fa: F[A], s: S)(f: A => State[S,B]): (F[B], S) =
    traverseS(fa)(f)(s)

  /** Traverse `fa` with a `State[S, G[B]]`, internally using a `Trampoline` to avoid stack overflow. */
  def traverseSTrampoline[S, G[_] : Applicative, A, B](fa: F[A])(f: A => State[S, G[B]]): State[S, G[F[B]]] = {
    import Free._
    implicit val A = StateT.stateTMonadState[S, Trampoline].compose(Applicative[G])
    traverse[({type λ[α]=StateT[Trampoline, S, G[α]]})#λ, A, B](fa)(f(_: A).lift[Trampoline]).unliftId[Trampoline]
  }

  /** Traverse `fa` with a `Kleisli[G, S, B]`, internally using a `Trampoline` to avoid stack overflow. */
  def traverseKTrampoline[S, G[_] : Applicative, A, B](fa: F[A])(f: A => Kleisli[G, S, B]): Kleisli[G, S, F[B]] = {
    import Free._
    implicit val A = Kleisli.kleisliMonadReader[Trampoline, S].compose(Applicative[G])
    Kleisli(traverse[({type λ[α]=Kleisli[Trampoline, S, G[α]]})#λ, A, B](fa)(z => Kleisli[Id, S, G[B]](i => f(z)(i)).lift[Trampoline]).unliftId[Trampoline] run _)
  }

  // derived functions
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] = 
    traversal[G].run[G[A], A](fga)(ga => ga)

  def sequenceS[S,A](fga: F[State[S,A]]): State[S,F[A]] = 
    traversalS[S].run(fga)(a => a)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traversal[Id](Id.id).run(fa)(f)

  def foldLShape[A,B](fa: F[A], z: B)(f: (B,A) => B): (F[Unit], B) = 
    runTraverseS(fa, z)(a => State(b => ((), f(b,a))))

  override def foldLeft[A,B](fa: F[A], z: B)(f: (B,A) => B): B = foldLShape(fa, z)(f)._2

  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B = foldLShape(fa, F.zero)((b, a) => F.append(b, f(a)))._2

  override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B) =
    foldMap(fa)((a: A) => (Endo.endo(f(a, _: B)))) apply z

  def reverse[A](fa: F[A]): F[A] = { 
    val (shape, as) = foldLShape(fa, scala.List[A]())((t,h) => h :: t)
    runTraverseS(shape, as)(_ => State(e => (e.head, e.tail)))._1
  }

  def zipWith[A,B,C](fa: F[A], fb: F[B])(f: (A, Option[B]) => C): (F[C], List[B]) = 
    runTraverseS(fa, toList(fb))(a =>
      State(bs => (f(a,bs.headOption), if (bs.isEmpty) bs else bs.tail)))

  def zipWithL[A,B,C](fa: F[A], fb: F[B])(f: (A,Option[B]) => C): F[C] = zipWith(fa, fb)(f)._1
  def zipWithR[A,B,C](fa: F[A], fb: F[B])(f: (Option[A],B) => C): F[C] = zipWith(fb, fa)((b,oa) => f(oa,b))._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] = zipWithL(fa, fb)((_,_))
  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] = zipWithR(fa, fb)((_,_))

  // mapAccumL, mapAccumR, map, filter?

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
      val t2: MN[F[C]] = traverse[MN, A, C](fa)(a => M.map(amb(a))(b => bnc(b)))(M compose N)
      MN.equal(t1, t2)
    }

    /** Traversal with the `point` function is the same as applying the `point` function directly */
    def purity[G[_], A](fa: F[A])(implicit G: Applicative[G], GFA: Equal[G[F[A]]]): Boolean = {
      GFA.equal(traverse[G, A, A](fa)(G.point[A](_)), G.point(fa))
    }

    /**
     * @param nat A natural transformation from `M` to `N` for which these properties hold:
     *            `(a: A) => nat(Pointed[M].pure[A](a)) === Pointed[M].point[A](a)`
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
  val traverseSyntax = new scalaz.syntax.TraverseSyntax[F] {}
}

object Traverse {
  @inline def apply[F[_]](implicit F: Traverse[F]): Traverse[F] = F

  ////

  ////
}

