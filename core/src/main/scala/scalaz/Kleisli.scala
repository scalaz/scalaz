package scalaz

sealed trait Kleisli[M[_], A, B] {
  def apply(a: A): M[B]

  import Scalaz._

  def >=>[C](k: Kleisli[M, B, C])(implicit b: Bind[M]): Kleisli[M, A, C] = ☆((a: A) => b.bind(this(a), k(_: B)))

  def >=>[C](k: B => M[C])(implicit b: Bind[M]): Kleisli[M, A, C] = >=>(☆(k))

  def <=<[C](k: Kleisli[M, C, A])(implicit b: Bind[M]): Kleisli[M, C, B] = k >=> this

  def <=<[C](k: C => M[A])(implicit b: Bind[M]): Kleisli[M, C, B] = ☆(k) >=> this

  def compose[N[_]](f: M[B] => N[B]): Kleisli[N, A, B] = ☆((a: A) => f(this(a)))

  def traverse[F[_], AA <: A](f: F[AA])(implicit a: Applicative[M], t: Traverse[F]): M[F[B]] =
    f ↦ (Kleisli.this(_)) 

  def =<<[AA <: A](a: M[AA])(implicit m: Bind[M]): M[B] = m.bind(a, apply _)  
}

trait Kleislis {
  def kleisli[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = new Kleisli[M, A, B] {
    def apply(a: A) = f(a)
  }

  def ☆[M[_], A, B](f: A => M[B]): Kleisli[M, A, B] = kleisli(f)

  implicit def kleisliFn[M[_],A,B](k: Kleisli[M,A,B]): A => M[B] = (a: A) => k(a)

  def kleisliPure[M[_], R](implicit m: Pure[M])
    : Pure[({type λ[x]=Kleisli[M, R, x]})#λ] =
  new Pure[({type λ[x]=Kleisli[M, R, x]})#λ] {
      def pure[A](a: => A) = kleisli((r: R) => m.pure(a))
    }

/*
  implicit def kleisliBind[M[_]:Bind, R]
    : Bind[({type λ[x]=Kleisli[M, R, x]})#λ] =
  new Bind[({type λ[x]=Kleisli[M, R, x]})#λ] {
      def bind[A,B](m: Kleisli[M, R, A], k: A => Kleisli[M, R, B]) =
        kleisli((r: R) => m.apply(r) >>= ((a: A) => k(a).apply(r)))
    }
*/

  implicit def kleisliBind[M[_], R](implicit b: Bind[M])
    : Bind[({type λ[x]=Kleisli[M, R, x]})#λ] =
  new Bind[({type λ[x]=Kleisli[M, R, x]})#λ] {
      def bind[A,B](m: Kleisli[M, R, A], k: A => Kleisli[M, R, B]) =
        kleisli((r: R) => b.bind[A,B](m.apply(r),((a: A) => k(a).apply(r))))
    }
}

