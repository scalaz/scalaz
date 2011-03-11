package scalaz

import Scalaz._

sealed trait Writer[W, A] extends NewType[(W, A)] {
  val value: (W, A)

  val written = value._1

  val over = value._2

  val toWriterT: WriterT[Identity, W, A] =
    writerT[Identity, W, A](value)

  def map[B](f: A => B): Writer[W, B] = new Writer[W, B] {
    val value = (Writer.this.written, f(Writer.this.over))
  }

  def flatMap[B](f: A => Writer[W, B])(implicit m: Semigroup[W]): Writer[W, B] = new Writer[W, B] {
    val value = {
      val k = f(Writer.this.over)
      (Writer.this.written |+| k.written, k.over)
    }
  }

  def foreach(f: A => Unit) =
    f(over)
}

object Writer {
  implicit def WriterInjective[W] = Injective[({type λ[α]= Writer[W, α]})#λ]

  implicit def WriterPure[W: Zero]: Pure[({type λ[α]= Writer[W, α]})#λ] = new Pure[({type λ[α]=Writer[W, α]})#λ] {
    def pure[A](a: => A) = new Writer[W, A] {
      val value = (∅[W], a)
    }
  }

  implicit def WriterFunctor[W]: Functor[({type λ[α]=Writer[W, α]})#λ] = new Functor[({type λ[α]= Writer[W, α]})#λ] {
    def fmap[A, B](x: Writer[W, A], f: A => B) =
      x map f
  }

  implicit def WriterApply[W](implicit ss: Semigroup[W]): Apply[({type λ[α]=Writer[W, α]})#λ] = new Apply[({type λ[α]=Writer[W, α]})#λ] {
    def apply[A, B](f: Writer[W, A => B], a: Writer[W, A]): Writer[W, B] = new Writer[W, B] {
      val value = {
        val (w1, ff) = f.value
        val (w2, aa) = a.value
        (w1 |+| w2, ff(aa))
      }
    }
  }

  implicit def WriterBind[W](implicit sg: Semigroup[W]): Bind[({type λ[α]=Writer[W, α]})#λ] = new Bind[({type λ[α]=Writer[W, α]})#λ] {
    def bind[A, B](a: Writer[W, A], f: A => Writer[W, B]) =
      a flatMap f
  }

  implicit def WriterEach[W]: Each[({type λ[α]=Writer[W, α]})#λ] = new Each[({type λ[α]= Writer[W, α]})#λ] {
    def each[A](x: Writer[W, A], f: A => Unit) =
      x foreach f
  }
}

trait Writers {
  def writer[W, A](w: W, a: A): Writer[W, A] = new Writer[W, A] {
    val value = (w, a)
  }
}
