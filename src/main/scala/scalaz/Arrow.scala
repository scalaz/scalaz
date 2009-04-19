package scalaz

trait Arrow[A[_, _]] {
  def arrow[B, C](f: B => C): A[B, C]

  def compose[B, C, D](a1: A[B, C], a2: A[C, D]): A[B, D]

  def first[B, C, D](a: A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: A[B, C]): A[(D, B), (D, C)]

  def ***[B, C, D, E](f: A[B, C], g: A[D, E]): A[(B, D), (C, E)] =
    compose(first(f), second(g))

  def &&&[B, C, D](f: A[B, C], g: A[B, D]): A[B, (C, D)] =
    compose(arrow((b: B) => (b, b)), ***(f, g))
}

object Arrow {
  implicit val Function1Arrow = new Arrow[Function1] {
    def arrow[B, C](f: B => C) = f

    def compose[B, C, D](a1: B => C, a2: C => D) =
      a2 compose a1

    def first[B, C, D](a: B => C) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: B => C) =
      (db: (D, B)) => (db._1, a(db._2))
  }

  def KleisliArrow[M[_]](implicit m: Monad[M]) =
    new Arrow[PartialApplyK[Kleisli, M]#Apply] {
      import Kleisli.kleisli

      def arrow[B, C](f: B => C) =
        kleisli[M]((b: B) => m.pure(f(b)))

      def compose[B, C, D](a1: Kleisli[M, B, C], a2: Kleisli[M, C, D]) =
        kleisli[M]((b: B) => m.bind(a1(b), (c: C) => a2(c)))

      def first[B, C, D](a: Kleisli[M, B, C]) =
        kleisli[M].apply[(B, D), (C, D)] {case (b, d) => m.fmap(a(b), (c: C) => (c, d))}

      def second[B, C, D](a: Kleisli[M, B, C]) =
        kleisli[M].apply[(D, B), (D, C)] {case (d, b) => m.fmap(a(b), (c: C) => (d, c))}
    }

  def CokleisliArrow[W[_]](implicit w: Comonad[W]) = new Arrow[PartialApplyK[Cokleisli, W]#Apply] {
    import Cokleisli.cokleisli

    def arrow[B, C](f: B => C) =
      cokleisli[W](f.compose(w.copure(_: W[B])))

    def compose[B, C, D](a1: Cokleisli[W, B, C], a2: Cokleisli[W, C, D]) =
      cokleisli[W]((e: W[B]) => a2(w.cobind(e, a1.apply(_: W[B]))))

    def first[B, C, D](a: Cokleisli[W, B, C]): Cokleisli[W, (B, D), (C, D)] =
      cokleisli[W].apply[(B, D), (C, D)](***(a, arrow(identity(_: D))).apply(_: W[(B, D)]))

    def second[B, C, D](a: Cokleisli[W, B, C]): Cokleisli[W, (D, B), (D, C)] =
      cokleisli[W].apply[(D, B), (D, C)](***(arrow(identity(_: D)), a).apply(_: W[(D, B)]))

    override def ***[B, C, D, E](a: Cokleisli[W, B, C], b: Cokleisli[W, D, E]): Cokleisli[W, (B, D), (C, E)] =
      &&&(cokleisli[W]((e: W[(B, D)]) => a(w.fmap(e, (_: (B, D))._1))),
        cokleisli[W]((h: W[(B, D)]) => b(w.fmap(h, (_: (B, D))._2))))
  }
}
