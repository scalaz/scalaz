package scalaz


sealed trait Const[A, B] {
  val value: A
}

object Const extends Consts

trait Consts {

  import ~>._

  def const[B]: (I ~> ({type λ[α] = Const[α, B]})#λ) = new (I ~> ({type λ[α] = Const[α, B]})#λ) {
    def apply[A](a: A) = new Const[A, B] {
      val value = a
    }
  }

  implicit def ConstTo[A, B](c: Const[A, B]): A =
    c.value

  implicit def Const_^*^[A, B]: ^*^[Const[A, B], A] =
    ^*^.^*^(_.value, a => new Const[A, B] {
      val value = a
    })

  implicit def ConstShow[B: Show, A]: Show[Const[B, A]] =
    Show.UnpackShow[Const[B, A], B]

  implicit def ConstEqual[B: Equal, A]: Equal[Const[B, A]] =
    Equal.UnpackEqual[Const[B, A], B]

  implicit def ConstOrder[B: Order, A]: Order[Const[B, A]] =
    Order.UnpackOrder[Const[B, A], B]

  implicit def ConstApplic[B: Semigroup]: Applic[({type λ[α] = Const[B, α]})#λ] = new Applic[({type λ[α] = Const[B, α]})#λ] {
    def applic[A, X](f: Const[B, A => X]) =
      fa =>
        const[X](implicitly[Semigroup[B]].append(f.value, fa.value))
  }

  implicit def ConstFunctor[B]: Functor[({type λ[α] = Const[B, α]})#λ] = new Functor[({type λ[α] = Const[B, α]})#λ] {
    def fmap[A, X](f: A => X) =
      r => const(r.value)
  }

  implicit def ConstPointed[A: Zero]: Pointed[({type λ[α] = Const[A, α]})#λ] = new Pointed[({type λ[α] = Const[A, α]})#λ] {
    def point[B](a: => B) = const[B](implicitly[Zero[A]].zero)
  }

  implicit def ConstPointedFunctor[A: Zero]: PointedFunctor[({type λ[α] = Const[A, α]})#λ] =
    PointedFunctor.pointedFunctor[({type λ[α] = Const[A, α]})#λ]

  implicit def ConstApplicative[A: Monoid] = {
    implicit val z = implicitly[Monoid[A]].zero
    implicit val s = implicitly[Monoid[A]].semigroup
    Applicative.applicative[({type λ[α] = Const[A, α]})#λ]
  }

  def liftConst[A, B](f: A => B): A => Const[B, A] = {a: A => Const.const(f(a))}

}