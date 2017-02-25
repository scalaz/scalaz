package scalaz

/**
 * @see [[https://github.com/ekmett/comonad/blob/v4.2.7.2/src/Control/Comonad/Trans/Traced.hs]]
 */
final case class TracedT[W[_], A, B](run: W[A => B]) {
  def map[C](f: B => C)(implicit W: Functor[W]): TracedT[W, A, C] =
    TracedT(W.map(run)(_ andThen f))

  def cobind[C](f: TracedT[W, A, B] => C)(implicit W: Cobind[W], A: Semigroup[A]): TracedT[W, A, C] =
    TracedT(
      W.extend(run) { wf => m =>
        f(TracedT(W.map(wf)(_.compose(A.append(_, m)))))
      }
    )

  def trans[M[_]](f: W ~> M): TracedT[M, A, B] =
    TracedT(f(run))

  def copoint(implicit W: Comonad[W], A: Monoid[A]): B =
    W.copoint(run).apply(A.zero)

  def lower(implicit W: Functor[W], A: Monoid[A]): W[B] =
    W.map(run)(_ apply A.zero)

  def contramap[C](f: C => A)(implicit W: Functor[W]): TracedT[W, C, B] =
    TracedT(W.map(run)(f.andThen))
}

sealed abstract class TracedTInstances5 {
  implicit final def tracedTFunctor[W[_]: Functor, C]: Functor[TracedT[W, C, ?]] =
    new TracedTFunctor[W, C]{
      def W = implicitly
    }

  implicit final def tracedTContravariant[W[_]: Functor, C]: Contravariant[TracedT[W, ?, C]] =
    new Contravariant[TracedT[W, ?, C]]{
      override def contramap[A, B](r: TracedT[W, A, C])(f: B => A) =
        r contramap f
    }
}

sealed abstract class TracedTInstances4 extends TracedTInstances5 {
  implicit final def tracedTDistributive[W[_]: Distributive, C]: Distributive[TracedT[W, C, ?]] =
    new TracedTDistributive[W, C] {
      def W = implicitly
    }
}

sealed abstract class TracedTInstances3 extends TracedTInstances4 {
  implicit final def tracedTApply[W[_]: Apply, C]: Apply[TracedT[W, C, ?]] =
    new TracedTApply[W, C]{
      def W = implicitly
    }
}

sealed abstract class TracedTInstances2 extends TracedTInstances3 {
  implicit final def tracedTApplicative[W[_]: Applicative, C]: Applicative[TracedT[W, C, ?]] =
    new TracedTApplicative[W, C]{
      def W = implicitly
    }
}

sealed abstract class TracedTInstances1 extends TracedTInstances2 {
  implicit final def tracedTCobind[W[_]: Cobind, C: Semigroup]: Cobind[TracedT[W, C, ?]] =
    new TracedTCobind[W, C]{
      def W = implicitly
      def C = implicitly
    }
}

sealed abstract class TracedTInstances0 extends TracedTInstances1 {

  implicit final def tracedTComonad[W[_]: Comonad, C: Monoid]: Comonad[TracedT[W, C, ?]] =
    new TracedTComonad[W, C]{
      def W = implicitly
      def C = implicitly
    }

  implicit final def tracedTCohoist[C: Monoid]: Cohoist[Lambda[(w[_], b) => TracedT[w, C, b]]] =
    new Cohoist[Lambda[(w[_], b) => TracedT[w, C, b]]] {
      override def cohoist[M[_], N[_]: Comonad](f: M ~> N) =
        λ[TracedT[M, C, ?] ~> TracedT[N, C, ?]](_ trans f)
      override def lower[G[_], A](a: TracedT[G, C, A])(implicit G: Cobind[G]) =
        a.lower
    }

  implicit final def tracedTEqual[W[_], A, B](implicit W: Equal[W[A => B]]): Equal[TracedT[W, A, B]] =
    W.contramap(_.run)

}

sealed abstract class TracedTInstances extends TracedTInstances0 {

  implicit final def tracedTComonadStore[W[_], S, C: Monoid](implicit W0: ComonadStore[W, S]): ComonadStore[TracedT[W, C, ?], S] =
    new ComonadStore[TracedT[W, C, ?], S] with TracedTComonad[W, C] {
      def W = W0
      def C = implicitly
      override def pos[A](w: TracedT[W, C, A]) =
        W0.pos(TracedT.tracedTCohoist[C].lower(w))
      override def peek[A](s: S, w: TracedT[W, C, A]) =
        W0.peek(s, TracedT.tracedTCohoist[C].lower(w))
    }

}

object TracedT extends TracedTInstances {

  def tracedTU[WAB, AB, A0, B0](wab: WAB)(implicit
    U1: Unapply[Functor, WAB]{type A = AB},
    U2: Unapply2[Profunctor, AB]{type A = A0; type B = B0},
    L: Leibniz.===[AB, A0 => B0]
  ): TracedT[U1.M, A0, B0] = TracedT(L.subst[U1.M](U1(wab)))

  import scalaz.Isomorphism._

  def iso[W[_]]: TracedT[W, ?, ?] <~~> Lambda[(a, b) => W[a => b]] =
    new IsoBifunctorTemplate[TracedT[W, ?, ?], Lambda[(a, b) => W[a => b]]] {
      override def to[A, B](fa: TracedT[W, A, B]) = fa.run
      override def from[A, B](ga: W[A => B]) = TracedT(ga)
    }

}

private trait TracedTFunctor[W[_], C] extends Functor[TracedT[W, C, ?]] {
  implicit def W: Functor[W]

  override final def map[A, B](fa: TracedT[W, C, A])(f: A => B) =
    fa map f
}

private trait TracedTDistributive[W[_], C] extends Distributive[TracedT[W, C, ?]] with TracedTFunctor[W, C] {
  def W: Distributive[W]

  import scalaz.std.function._

  override final def distributeImpl[G[_], A, B](fa: G[A])(f: A => TracedT[W, C, B])(implicit G: Functor[G]) =
    TracedT(
      W.map(W.cosequence(G.map(fa)(f(_).run))){
        Distributive[C => ?].cosequence(_)
      }
    )
}

private trait TracedTApply[W[_], C] extends Apply[TracedT[W, C, ?]] with TracedTFunctor[W, C] {
  def W: Apply[W]

  override final def ap[A, B](fa: => TracedT[W, C, A])(f: => TracedT[W, C, A => B]) =
    TracedT(
      W.ap(fa.run)(
        W.map(f.run)(cab => ca => c => cab(c).apply(ca(c)))
      )
    )
}

private trait TracedTApplicative[W[_], C] extends Applicative[TracedT[W, C, ?]] with TracedTApply[W, C] {
  def W: Applicative[W]

  override final def point[A](a: => A) = TracedT(W.point(Function.const(a)))
}

private trait TracedTCobind[W[_], C] extends Cobind[TracedT[W, C, ?]] with TracedTFunctor[W, C] {
  implicit def W: Cobind[W]
  implicit def C: Semigroup[C]

  override final def cobind[A, B](fa: TracedT[W, C, A])(f: TracedT[W, C, A] => B) =
    fa cobind f
}

private trait TracedTComonad[W[_], C] extends Comonad[TracedT[W, C, ?]] with TracedTCobind[W, C] {
  implicit def W: Comonad[W]
  implicit def C: Monoid[C]

  override final def copoint[A](p: TracedT[W, C, A]): A =
    p.copoint
}
