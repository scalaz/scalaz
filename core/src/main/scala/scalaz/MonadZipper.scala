package scalaz

object Introduction {
  case class Var[A](value: String)
  case class Add[E](l: E, r: E)
  sealed trait :+:[F[_],G[_],A]
  case class Inl[F[_],G[_],A](value: F[A]) extends :+:[F,G,A]
  case class Inr[F[_],G[_],A](value: G[A]) extends :+:[F,G,A]
  case class Fix[F[_]](value: F[Fix[F]]) extends NewType[F[Fix[F]]]

  trait PApp[M[_[_],_[_],_],F[_],G[_]] { type Apply[A] = M[F,G,A] }

  type Expr0 = Fix[PApp[:+:,Add,Var]#Apply]
}
