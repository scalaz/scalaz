package scalaz

final case class Const[A, B](getConst: A)

private sealed trait ConstSemigroup[A, B] extends Semigroup[Const[A, B]] {
  def A: Semigroup[A]

  override def append(f1: Const[A, B], f2: => Const[A, B]): Const[A, B] =
    Const(A.append(f1.getConst, f2.getConst))
}

private sealed trait ConstMonoid[A, B] extends Monoid[Const[A, B]] with ConstSemigroup[A, B] {
  def A: Monoid[A]

  override def zero: Const[A, B] =
    Const(A.zero)
}

private sealed trait ConstTraverse[C] extends Traverse[Const[C, *]] {
  override def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] = Const(fa.getConst)

  override def traverseImpl[G[_], A, B](fa: Const[C, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.point(Const(fa.getConst))
}

private sealed trait ConstApplyDivide[C] extends Apply[Const[C, *]] with ConstTraverse[C] with ConstContravariant[C] with Divide[Const[C, *]] {
  def C: Semigroup[C]

  override def tuple2[A, B](fa: =>Const[C, A], fb: =>Const[C, B]): Const[C, (A, B)] = apply2(fa, fb)((_,_))

  override def xmap[A, B](fa: Const[C, A], f: A => B, g: B => A) =
    Const(fa.getConst)

  override def ap[A, B](fa: => Const[C, A])(f: => Const[C, A => B]): Const[C, B] =
    Const(C.append(f.getConst, fa.getConst))

  override def divide2[A, B, Z](fa: =>Const[C, A], fb: =>Const[C, B])(f: Z => (A, B)) =
    Const(C.append(fa.getConst, fb.getConst))
}

private sealed trait ConstApplicativeDivisible[C] extends Applicative[Const[C, *]] with ConstApplyDivide[C] with Divisible[Const[C, *]] {
  def C: Monoid[C]

  override def point[A](a: => A): Const[C, A] = Const(C.zero)

  override def conquer[A] = Const(C.zero)
}

private sealed trait ConstEqual[A, B] extends Equal[Const[A, B]] {
  def OA: Equal[A]

  override def equal(a1: Const[A, B], a2: Const[A, B]): Boolean =
    OA.equal(a1.getConst, a2.getConst)

  override def equalIsNatural = OA.equalIsNatural
}

private sealed trait ConstOrder[A, B] extends Order[Const[A, B]] with ConstEqual[A, B] {
  def OA: Order[A]

  override def order(a1: Const[A, B], a2: Const[A, B]): Ordering =
    OA.order(a1.getConst, a2.getConst)
}

private trait ConstContravariant[C] extends Contravariant[Const[C, *]] {
  def contramap[A, B](r: Const[C, A])(f: B => A) =
    Const(r.getConst)
}

private sealed trait ConstShow[A, B] extends Show[Const[A, B]] {
  implicit def A: Show[A]

  import syntax.show._
  def show(f: Const[A, B]): Cord = cord"Const(${f.getConst})"
}

sealed abstract class ConstInstances1 {
  implicit def constTraverse[C]: Traverse[Const[C, *]] =
    new ConstTraverse[C] {}

  implicit def constContravariant[C]: Contravariant[Const[C, *]] =
    new ConstContravariant[C]{}
}

sealed abstract class ConstInstances0 extends ConstInstances1 {
  implicit def constEqual[A : Equal, B]: Equal[Const[A, B]] =
    new ConstEqual[A, B] {
      val OA: Equal[A] = implicitly
    }

  implicit def constSemigroup[A: Semigroup, B]: Semigroup[Const[A, B]] =
    new ConstSemigroup[A, B] {
      val A: Semigroup[A] = implicitly
    }

  implicit def constInstance1[C: Semigroup]: Apply[Const[C, *]] with Divide[Const[C, *]] =
    new ConstApplyDivide[C] {
      val C: Semigroup[C] = implicitly
    }
}

sealed abstract class ConstInstances extends ConstInstances0 {
  implicit def constOrder[A : Order, B]: Order[Const[A, B]] =
    new ConstOrder[A, B]{
      val OA: Order[A] = implicitly
    }

  implicit def constMonoid[A: Monoid, B]: Monoid[Const[A, B]] =
    new ConstMonoid[A, B] {
      val A: Monoid[A] = implicitly
    }

  implicit def constInstance2[C: Monoid]: Applicative[Const[C, *]] with Divisible[Const[C, *]] =
    new ConstApplicativeDivisible[C] {
      val C: Monoid[C] = implicitly
    }

  implicit def constShow[A: Show, B]: Show[Const[A, B]] =
    new ConstShow[A, B] {
      val A: Show[A] = implicitly
    }
}

object Const extends ConstInstances {

  /** A properly universally quantified constant function. */
  def const[A](a: A): Function0 ~> λ[α => A] =
    new (Function0 ~> λ[α => A]) {
      override def apply[B](fa: Function0[B]): A = a
    }
}
