package scalaz

trait Arrow[A[_, _]] {
  val category: Category[A]
  
  def arrow[B, C](f: B => C): A[B, C]

  def first[B, C, D](a: A[B, C]): A[(B, D), (C, D)]

  def second[B, C, D](a: A[B, C]): A[(D, B), (D, C)]
}

object Arrow {
  import Scalaz._
  
  implicit def Function1Arrow: Arrow[Function1] = new Arrow[Function1] {
    val category = Category.Function1Category
    
    def arrow[B, C](f: B => C) = f

    def first[B, C, D](a: B => C) =
      (bd: (B, D)) => (a(bd._1), bd._2)

    def second[B, C, D](a: B => C) =
      (db: (D, B)) => (db._1, a(db._2))
  }

  implicit def KleisliArrow[M[_]: Monad]: Arrow[PartialApplyK[Kleisli, M]#Apply] = new Arrow[PartialApplyK[Kleisli, M]#Apply] {
    val category = Category.KleisliCategory

    def arrow[B, C](f: B => C) = ☆(f(_) η)

    def first[B, C, D](a: Kleisli[M, B, C]) = ☆ {
      case (b, d) => a(b) ∘ ((_, d))
    }

    def second[B, C, D](a: Kleisli[M, B, C]) = ☆ {
      case (d, b) => a(b) ∘ ((d, _))
    }
  }

  implicit def CokleisliArrow[M[_]: Comonad]: Arrow[PartialApplyK[Cokleisli, M]#Apply] = new Arrow[PartialApplyK[Cokleisli, M]#Apply] {
    val category = Category.CokleisliCategory

    def arrow[B, C](f: B => C) = ★(r => f(r ε))

    // todo higher-order unification
    def first[B, C, D](a: Cokleisli[M, B, C]) = ★(mab[PartialApplyK[Cokleisli, M]#Apply, B, C](a) *** arrow(identity(_: D)) apply (_: M[(B, D)]))

    // todo higher-order unification
    def second[B, C, D](a: Cokleisli[M, B, C]) = ★(mab[PartialApplyK[Cokleisli, M]#Apply, D, D](arrow(identity(_: D))) *** a apply (_: M[(D, B)]))
  }
}
