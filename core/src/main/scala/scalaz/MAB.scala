package scalaz


sealed trait MAB[M[_, _], A, B] extends PimpedType[M[A, B]] with MA[({type λ[X]=M[A,X]})#λ, B] {
  def asMAB: MAB[M, A, B] = this

  def :->[D](g: B => D)(implicit b: Bifunctor[M]): M[A, D] = b.bimap(value, identity[A], g)

  def <-:[C](f: A => C)(implicit b: Bifunctor[M]): M[C, B] = b.bimap(value, f, identity[B])

  def >>>[C](k: M[B, C])(implicit c: Category[M]): M[A, C] = c compose (k, value)
 
  def ⋙[C](k: M[B, C])(implicit c: Category[M]): M[A, C] = c compose (k, value)

  def <<<[C](k: M[C, A])(implicit c: Category[M]): M[C, B] = c compose (value, k)
 
  def ⋘[C](k: M[C, A])(implicit c: Category[M]): M[C, B] = c compose (value, k)

  def first[C](implicit a: Arrow[M]): M[(A, C), (B, C)] = a first value

  def second[C](implicit a: Arrow[M]): M[(C, A), (C, B)] = a second value

  def ***[C, D](k: M[C, D])(implicit a: Arrow[M]): M[(A, C), (B, D)] = a.category.compose(a.second[C, D, B](k), first[C])

  def &&&[C](k: M[A, C])(implicit a: Arrow[M]): M[A, (B, C)] = a.category.compose(***(k), a.arrow(a => (a, a)))

  def product(implicit a: Arrow[M]): M[(A, A), (B, B)] = this *** value

  def ^>>[C](f: C => A)(implicit a: Arrow[M]): M[C, B] = a.category.compose(value, a.arrow(f))

  def >>^[C](f: B => C)(implicit a: Arrow[M]): M[A, C] = a.category.compose(a.arrow(f), value)

  def <<^[C](f: C => A)(implicit a: Arrow[M]): M[C, B] = a.category.compose(value, a.arrow(f))

  def ^<<[C](f: B => C)(implicit a: Arrow[M]): M[A, C] = a.category.compose(a.arrow(f), value)
}

// Implicit conversions to MA and MAB conversions must be prioritized.
// See https://github.com/scalaz/scalaz/issues/39
trait MABLow {
  implicit def mab[M[_, _], A, B](a: M[A, B]): MAB[M, A, B] = new MAB[M, A, B] {
    val value = a
  }
}

// Prefer conversion to MA over MAB
trait MAsLow extends MABLow {
  implicit def maImplicit[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val value = a
  }

  implicit def maContravariantImplicit[M[_], A](a: M[A]): MAContravariant[M, A] = new MAContravariant[M, A] {
    val value = a
  }
}

trait MABs extends MAsLow {
  def ma[M[_], A](a: M[A]): MA[M, A] = new MA[M, A] {
    val value = a
  }

  def maContravariant[M[_], A](a: M[A])(implicit cf: Contravariant[M]): MAContravariant[M, A] = new MAContravariant[M, A] {
    val value = a
  }

  //
  // Specific conversions to MAB
  //

  implicit def KleisliMAB[M[_], A, B](k: Kleisli[M, A, B]) = mab[({type λ[α, β]=Kleisli[M, α, β]})#λ, A, B](k)

  implicit def CokleisliMAB[M[_], A, B](k: Cokleisli[M, A, B]) = mab[({type λ[α, β]=Cokleisli[M, α, β]})#λ, A, B](k)

  implicit def Const2MAB[M, A, B](k: Const2[M,A,B]) = mab[({type λ[α, β]=Const2[M, α, β]})#λ, A, B](k)

  //
  // Specific conversions to MA
  //

  implicit def EitherLeftMA[X, A](a: Either.LeftProjection[A, X]) = ma[({type λ[α]=Either.LeftProjection[α, X]})#λ, A](a)

  implicit def EitherRightMA[X, A](a: Either.RightProjection[X, A]) = ma[({type λ[α]=Either.RightProjection[X, α]})#λ, A](a)

  implicit def Function1FlipMAContravariant[A, R](f: R => A) = maContravariant[({type λ[α]=(α) => A})#λ, R](f)

  implicit def Function1ApplyMA[A, R](f: A => R) = ma[({type λ[α]=(A) => α})#λ, R](f)

  implicit def Function2MA[R, S, A](a: (R, S) => A) = ma[({type λ[α]=(R, S) => α})#λ, A](a)

  implicit def Function3MA[R, S, T, A](a: (R, S, T) => A) = ma[({type λ[α]=(R, S, T) => α})#λ, A](a)

  implicit def Function4MA[R, S, T, U, A](a: (R, S, T, U) => A) = ma[({type λ[α]=(R, S, T, U) => α})#λ, A](a)

  implicit def Function5MA[R, S, T, U, V, A](a: (R, S, T, U, V) => A) = ma[({type λ[α]=(R, S, T, U, V) => α})#λ, A](a)

  implicit def Function6MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W) => A) = ma[({type λ[α]=(R, S, T, U, V, W) => α})#λ, A](a)

  implicit def ConstMA[B, A](c: Const[B, A]) = ma[({type λ[α]=Const[B, α]})#λ, A](c)

  implicit def StateMA[S, A](s: State[S, A]) = ma[({type λ[α]=State[S, α]})#λ, A](s)

  import effects._
  implicit def STMA[S, A](s: ST[S, A]) = ma[({type λ[α]=ST[S, α]})#λ, A](s)

  implicit def Tuple2MA[R, A](a: (R, A)) = ma[({type λ[α]=(R, α)})#λ, A](a)

  implicit def Tuple3MA[R, S, A](a: (R, S, A)) = ma[({type λ[α]=(R, S, α)})#λ, A](a)

  implicit def Tuple4MA[R, S, T, A](a: (R, S, T, A)) = ma[({type λ[α]=(R, S, T, α)})#λ, A](a)

  implicit def Tuple5MA[R, S, T, U, A](a: (R, S, T, U, A)) = ma[({type λ[α]=(R, S, T, U, α)})#λ, A](a)

  implicit def Tuple6MA[R, S, T, U, V, A](a: (R, S, T, U, V, A)) = ma[({type λ[α]=(R, S, T, U, V, α)})#λ, A](a)

  implicit def Tuple7MA[R, S, T, U, V, W, A](a: (R, S, T, U, V, W, A)) = ma[({type λ[α]=(R, S, T, U, V, W, α)})#λ, A](a)

  implicit def ValidationMA[A, E](v: Validation[E, A]) = ma[({type λ[α]=Validation[E, α]})#λ, A](v)

  implicit def ValidationFailureMA[A, E](f: FailProjection[E, A]) = ma[({type λ[α]=FailProjection[α, A]})#λ, E](f)

  implicit def IterVMA[A, E](v: IterV[E, A]) = ma[({type λ[α]=IterV[E, α]})#λ, A](v)

  import java.util.Map.Entry

  implicit def MapMA[K, V](m: Map[K, V]): MA[({type λ[α] = Map[α, V]})#λ, K] = ma[({type λ[α] = Map[α, V]})#λ, K](m)

  implicit def MapEntryMA[X, A](e: Entry[X, A]) = ma[({type λ[α]=Entry[X, α]})#λ, A](e)

  // Seq[A] implements Function1[Int, A]. Without this, Function1ApplyMA would be used.
  implicit def SeqMA[M[X] <: Seq[X], A](l: M[A]) = ma[M, A](l)

  // Set[A] implements Function1[Int, B]. Without this, Function1ApplyMA would be used.
  implicit def SetMA[M[X] <: Set[X], A](s: M[A]) = ma[M, A](s)

  implicit def KleisliMA[M[_], A, B](k: Kleisli[M,A,B]) = ma[({type λ[α]=Kleisli[M, A, α]})#λ, B](k)

  implicit def FingerTreeMA[V, A](t: FingerTree[V, A]) = ma[({type λ[α]=FingerTree[V, α]})#λ, A](t)

  implicit def FingerMA[V, A](t: Finger[V, A]) = ma[({type λ[α]=Finger[V, α]})#λ, A](t)

  implicit def NodeMA[V, A](t: Node[V, A]) = ma[({type λ[α]=Node[V, α]})#λ, A](t)

  implicit def MemoMA[V, A](m: Memo[A, V]) = ma[({type λ[α]=Memo[α, V]})#λ, A](m)
}
