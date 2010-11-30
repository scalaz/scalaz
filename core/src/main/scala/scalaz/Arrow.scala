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

  implicit def PartialFunctionArrow: Arrow[PartialFunction] = new Arrow[PartialFunction] {
    val category = Category.PartialFunctionCategory

    def arrow[B, C](f: B => C) = {
      case b => f(b)
    }

    def first[B, C, D](a: PartialFunction[B, C]) = {
      case (b, d) if a isDefinedAt b => (a(b), d)
    }

    def second[B, C, D](a: PartialFunction[B, C]): PartialFunction[(D, B), (D, C)] = {
      case (d, b) if a isDefinedAt b => (d, a(b))
    }
  }

  implicit def KleisliArrow[M[_]: Monad]: Arrow[({type λ[α, β]=Kleisli[M, α, β]})#λ] = new Arrow[({type λ[α, β]=Kleisli[M, α, β]})#λ] {
    val category = Category.KleisliCategory

    def arrow[B, C](f: B => C) = ☆(f(_) η)

    def first[B, C, D](a: Kleisli[M, B, C]) = ☆ {
      case (b, d) => a(b) ∘ ((_, d))
    }

    def second[B, C, D](a: Kleisli[M, B, C]) = ☆ {
      case (d, b) => a(b) ∘ ((d, _))
    }
  }

  implicit def CokleisliArrow[M[_]: Comonad]: Arrow[({type λ[α, β]=Cokleisli[M, α, β]})#λ] = new Arrow[({type λ[α, β]=Cokleisli[M, α, β]})#λ] {
    val category = Category.CokleisliCategory

    def arrow[B, C](f: B => C) = ★(r => f(r ε))

    def first[B, C, D](a: Cokleisli[M, B, C]) = ★(a *** arrow(identity(_: D)) apply (_: M[(B, D)]))

    def second[B, C, D](a: Cokleisli[M, B, C]) = ★(arrow(identity(_: D)) *** a apply (_: M[(D, B)]))
  }
}
