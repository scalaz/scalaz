package scalaz

trait Kleisli[M[_], -A, B] {
  def apply(a: A): M[B]

  def |=>(a: A) = apply(a)

  import Kleisli.kleisli

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]) = kleisli[M]((a: A) => b.bind(this(a), k(_: B)))

  def >=>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = >=>(kleisli[M](k))

  def compose[N[_]](f: M[B] => N[B]) = kleisli[N]((a: A) => f(this(a)))

  trait TraverseK[F[_]] {
    def apply[AA <: A](f: F[AA])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]]
  }

  def traverse[F[_]] = new TraverseK[F] {
    def apply[AA <: A](f: F[AA])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]] = t.traverse[M, AA, B](Kleisli.this(_), f)
  }

  def =<<[AA <: A](a: M[AA])(implicit m: Bind[M]) = m.bind(a, apply)
}

object Kleisli {
  sealed trait KleisliApply[M[_]] {
    def apply[A, B](f: A => M[B]): Kleisli[M, A, B]
  }

  def kleisli[M[_]] = new KleisliApply[M] {
    def apply[A, B](f: A => M[B]) = new Kleisli[M, A, B] {
      def apply(a: A) = f(a)
    }
  }

  implicit def KleisliF[W[_], A, B](c: Cokleisli[W, A, B]): (A => W[B]) = c.apply(_:A)
}