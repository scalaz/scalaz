package scalaz

sealed trait Kleisli[M[_], A, B] {
  def apply(a: A): M[B]

  import Scalaz._

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] = ☆((a: A) => b.bind(this(a), k(_: B)))

  def >=>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = >=>(☆(k))

  def <=<[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def <=<[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = ☆(k) >=> this

  def compose[N[_]](f: M[B] => N[B]) = ☆((a: A) => f(this(a)))

  def traverse[F[_], AA <: A](f: F[AA])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]] =
    f ↦ (Kleisli.this(_)) 

  def =<<[AA <: A](a: M[AA])(implicit m: Bind[M]): M[B] = m.bind(a, apply)  
}

trait Kleislis {
  def ☆[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = new Kleisli[M, A, B] {
    def apply(a: A) = f(a)
  }

  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = ☆(f)

  implicit def kleisliFn[M[_],A,B](k: Kleisli[M,A,B]): A => M[B] = (a: A) => k(a)
}
