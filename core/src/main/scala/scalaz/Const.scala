package scalaz


sealed trait Const[A, B] {
  val value: A

  import Const._

  def map[X](f: A => X): Const[A, X] =
    Const.const(value)
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

  def liftConst[A, B](f: A => B): A => Const[B, A] = {
    a: A => Const.const(f(a))
  }

}