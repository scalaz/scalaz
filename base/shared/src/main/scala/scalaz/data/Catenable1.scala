package scalaz
package data

trait Catenable1Module {
  type Catenable1[A]

  def foldBalanced[A: Semigroup](fa: Catenable1[A]): A

  def foldRight[A, B](fa: Catenable1[A], z: => B)(f: (A, => B) => B): B

  def foldLeft[A, B](fa: Catenable1[A], z: B)(f: (B, A) => B): B

  def cons[A](a: A, cat: Catenable1[A]): Catenable1[A]
  def snoc[A](cat: Catenable1[A], a: A): Catenable1[A]
  def concat[A](as: Catenable1[A], bs: Catenable1[A]): Catenable1[A]

  def single[A](a: A): Catenable1[A]

  implicit def isCovariantInstance: IsCovariant[Catenable1]
}

private[data] object Catenable1Impl extends Catenable1Module with BiconstInstances {
  type Catenable1[A] = ACatenable1[Biconst[A, ?, ?], Unit, Unit]

  def foldBalanced[A: Semigroup](fa: Catenable1[A]): A =
    Biconst.runBiconst(fa.fold)

  def foldRight[A, B](fa: Catenable1[A], z: => B)(f: (A, => B) => B): B = {
    val act = ν[LeftAction[λ[X => B], Biconst[A, ?, ?]]][α, β] { (a, b) => 
      f(Biconst.runBiconst(a), b)
    }
    fa.foldRight[λ[X => B]](z)(act)
  }

  def foldLeft[A, B](fa: Catenable1[A], z: B)(f: (B, A) => B): B = {
    val act = ν[RightAction[λ[X => B], Biconst[A, ?, ?]]][α, β] { (b, a) =>
      f(b, Biconst.runBiconst(a))
    }
    fa.foldLeft[λ[X => B]](z)(act)
  }

  def cons[A](a: A, cat: Catenable1[A]): Catenable1[A] = Biconst[A, Unit, Unit](a) +: cat
  def snoc[A](cat: Catenable1[A], a: A): Catenable1[A] = cat :+ Biconst[A, Unit, Unit](a)
  def concat[A](as: Catenable1[A], bs: Catenable1[A]): Catenable1[A] = bs compose as

  def single[A](a: A): Catenable1[A] = ACatenable1.lift(Biconst(a))

  implicit def isCovariantInstance: IsCovariant[Catenable1] = ???
}