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

  import Logger.LOG
  import Scalaz._

  def withWriter(k: Writer[LOG[A], B] => Writer[LOG[A], B])(implicit log: Logger[M]): M[A, B] =
    log fromWriter k(log toWriter asMAB)

  def withLog(k: LOG[A] => LOG[A])(implicit log: Logger[M]): M[A, B] =
    withWriter(w => w.over set k(w.written))

  def withEachLog(k: A => A)(implicit log: Logger[M]): M[A, B] =
    withLog(_ ∘ k)

  def setLog(l: LOG[A])(implicit log: Logger[M]): M[A, B] =
    withLog(_ => l)

  def :+->(e: A)(implicit log: Logger[M]): M[A, B] =
    withLog(_ |+| e.η[LOG])

  def <-+:(e: A)(implicit log: Logger[M]): M[A, B] =
    withLog(e.η[LOG] |+| _)

  def :++->(e: LOG[A])(implicit log: Logger[M]): M[A, B] =
    withLog(_ |+| e)

  def <-++:(e: LOG[A])(implicit log: Logger[M]): M[A, B] =
    withLog(e |+| _)

  def resetLog(implicit log: Logger[M]): M[A, B] =
    withLog(_ => ∅[LOG[A]])

  // CAUTION: side-effect
  def flushLog(k: LOG[A] => Unit)(implicit log: Logger[M]): M[A, B] = {
    k((log toWriter asMAB).written)
    resetLog
  }

  // CAUTION: side-effect
  def flushEachLog(k: A => Unit)(implicit log: Logger[M]): M[A, B] = {
    flushLog(_ ∘ k)
  }
}

trait MABLow {
  implicit def mab[M[_, _], A, B](a: M[A, B]): MAB[M, A, B] = new MAB[M, A, B] {
    val value = a
  }
}

trait MABs extends MABLow {
  implicit def KleisliMAB[M[_], A, B](k: Kleisli[M, A, B]) = mab[({type λ[α, β]=Kleisli[M, α, β]})#λ, A, B](k)

  implicit def CokleisliMAB[M[_], A, B](k: Cokleisli[M, A, B]) = mab[({type λ[α, β]=Cokleisli[M, α, β]})#λ, A, B](k)

  implicit def Const2MAB[M, A, B](k: Const2[M,A,B]) = mab[({type λ[α, β]=Const2[M, α, β]})#λ, A, B](k)
}
