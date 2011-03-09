package scalaz

sealed trait OptionT[M[_], A] extends NewType[M[Option[A]]] {
  def map[B](f: A => B)(implicit ftr: Functor[M]): OptionT[M, B] = new OptionT[M, B] {
    val value = ftr.fmap(OptionT.this.value, (x: Option[A]) => x map f)
  }

  def flatMap[B](f: A => OptionT[M, B])(implicit mnd: Monad[M]): OptionT[M, B] = new OptionT[M, B] {
    val value = mnd.bind(OptionT.this.value, (a: Option[A]) =>
      a match {
        case None => mnd.pure(None: Option[B])
        case Some(z) => f(z).value
      })
  }
}

object OptionT {
  import Scalaz._
  implicit def OptionTInjective[M[_]] = Injective[({type λ[α]= OptionT[M, α]})#λ]

  implicit def OptionTPure[M[_]: Pure]: Pure[({type λ[α]= OptionT[M, α]})#λ] = new Pure[({type λ[α]=OptionT[M, α]})#λ] {
    def pure[A](a: => A) = new OptionT[M, A] {
      val value = a.η[Option].η[M]
    }
  }

  implicit def OptionTFunctor[M[_]: Functor]: Functor[({type λ[α]=OptionT[M, α]})#λ] = new Functor[({type λ[α]= OptionT[M, α]})#λ] {
    def fmap[A, B](x: OptionT[M, A], f: A => B) = x map f
  }

  implicit def OptionTApply[M[_]](implicit ma: Apply[M], ftr: Functor[M]): Apply[({type λ[α]=OptionT[M, α]})#λ] = new Apply[({type λ[α]=OptionT[M, α]})#λ] {
    def apply[A, B](f: OptionT[M, A => B], a: OptionT[M, A]): OptionT[M, B] = new OptionT[M, B] {
      val value = f.value.<**>(a.value) { case (ff, aa) => aa <*> ff }
    }

  }

  implicit def OptionTBind[M[_]](implicit mnd: Monad[M]): Bind[({type λ[α]=OptionT[M, α]})#λ] = new Bind[({type λ[α]=OptionT[M, α]})#λ] {
    def bind[A, B](a: OptionT[M, A], f: A => OptionT[M, B]) =
      a flatMap f
  }

  implicit def OptionTEach[M[_]: Each]: Each[({type λ[α]=OptionT[M, α]})#λ] = new Each[({type λ[α]= OptionT[M, α]})#λ] {
    def each[A](x: OptionT[M, A], f: A => Unit) = x.value foreach (_ foreach f)
  }
}

trait OptionTs {
  import Scalaz._
  def optionT[M[_]] = new (({type λ[α]=M[Option[α]]})#λ ~> ({type λ[α]=OptionT[M, α]})#λ) {
    def apply[A](a: M[Option[A]]) = new OptionT[M, A] {
      val value = a
    }
  }
}