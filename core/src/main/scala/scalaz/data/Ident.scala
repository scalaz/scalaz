package scalaz
package data

sealed trait Ident[A] {
  def value: A

  def map[B](f: A => B): Ident[B] = new Ident[B] {
    def value = f(Ident.this.value)
  }
}

object Ident extends Idents {
  def apply[A]: (=> A) => Ident[A] =
    ident[A]
}

trait Idents {
  def ident[A]: (=> A) => Ident[A] = v => new Ident[A] {
    def value = v
  }

  implicit val IdentFunctor: Functor[Ident] = new Functor[Ident] {
    def fmap[A, B](f: A => B) = a => ident(f(a.value))
  }

  implicit val IdentPointed: Pointed[Ident] = new Pointed[Ident] {
    def point[A](a: => A) = ident(a)
  }

  implicit val IdentPointedFunctor: PointedFunctor[Ident] =
    PointedFunctor.pointedFunctor[Ident]

  implicit val IdentBind: Bind[Ident] = new Bind[Ident] {
    def bind[A, B](f: A => Ident[B]) = a => ident(f(a.value).value)
  }

  implicit val IdentMonad: Monad[Ident] =
    Monad.monadBP[Ident]

  implicit val IdentApplic: Applic[Ident] = IdentMonad.applic

  implicit val IdentApplicative: Applicative[Ident] = IdentMonad.applicative

  implicit val IdentJoin: Join[Ident] = IdentMonad.join
}
