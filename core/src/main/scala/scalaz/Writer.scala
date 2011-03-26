package scalaz

import Scalaz._

sealed trait Writer[W, A] extends NewType[(W, A)] {
  val value: (W, A)

  def written = value._1

  def over = value._2

  def toWriterT: WriterT[Identity, W, A] =
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

  implicit val WriterBifunctor: Bifunctor[Writer] = new Bifunctor[Writer] {
    def bimap[A, B, C, D](k: Writer[A, B], f: A => C, g: B => D): Writer[C, D] =
      writer(f(k.written), g(k.over))
  }

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

  implicit def WriterIndex[W]: Index[({type λ[α]=Writer[W, α]})#λ] = new Index[({type λ[α]=Writer[W, α]})#λ] {
    def index[A](a: Writer[W, A], n: Int) =
      if(n == 0) Some(a.over) else None
  }

  implicit def WriterFoldable[W]: Foldable[({type λ[α]=Writer[W, α]})#λ] = new Foldable[({type λ[α]=Writer[W, α]})#λ] {
    override def foldRight[A, B](t: Writer[W, A], b: => B, f: (A, => B) => B) =
      f(t.over, b)
  }

  implicit def WriterTraverse[W]: Traverse[({type λ[α]=Writer[W, α]})#λ] = new Traverse[({type λ[α]=Writer[W, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: Writer[W, A]) =
      f(t.over) ∘ (b => new Writer[W, B] {
        val value = (t.written, b)
      })
  }

  implicit def WriterShow[W : Show, A : Show]: Show[Writer[W, A]] = new Show[Writer[W, A]] {
    def show(a: Writer[W, A]) =
      ("Writer(" + a.written.shows + "," + a.over.shows + ")").toList
  }

  implicit def WriterEqual[W, A: Equal]: Equal[Writer[W, A]] = new Equal[Writer[W, A]] {
    def equal(a1: Writer[W, A], a2: Writer[W, A]) =
      a1.over === a2.over
  }

  implicit def WriterOrder[W, A: Order]: Order[Writer[W, A]] = new Order[Writer[W, A]] {
    def order(a1: Writer[W, A], a2: Writer[W, A]) =
      a1.over ?|? a2.over
  }

  implicit def WriterZero[W : Zero, A: Zero]: Zero[Writer[W, A]] = new Zero[Writer[W, A]] {
    val zero = new Writer[W, A] {
      val value = (∅[W], ∅[A])
    }
  }
}

trait Writers {
  def writer[W, A](w: W, a: A): Writer[W, A] = new Writer[W, A] {
    val value = (w, a)
  }
}
