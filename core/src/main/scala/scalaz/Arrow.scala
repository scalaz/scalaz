package scalaz

trait Arrow[F[_, _]] {
  val category: Category[F]
  val arr: Arr[F]
  val first: First[F]

  def id[A]: F[A, A] =
    category.i[A]

  def functor[C]: Functor[({type λ[α] = F[C, α]})#λ] =
    new Functor[({type λ[α] = F[C, α]})#λ] {
      def fmap[A, B](f: A => B) =
        <<<(ar(f))(_)
    }

  def fmap[A, B, C](f: A => B): F[C, A] => F[C, B] =
    functor.fmap(f)

  def applic[C]: Applic[({type λ[α] = F[C, α]})#λ] =
    new Applic[({type λ[α] = F[C, α]})#λ] {
      def applic[A, B](f: F[C, A => B]) =
        (k: F[C, A]) =>
          <<<(ar((y: (A => B, A)) => y._1(y._2)))(combine(f)(k))
    }

  def apply[A, B, C](f: F[C, A => B]): F[C, A] => F[C, B] =
    applic.applic(f)

  def applicFunctor[C]: ApplicFunctor[({type λ[α] = F[C, α]})#λ] =
    new ApplicFunctor[({type λ[α] = F[C, α]})#λ] {
      val applic = Arrow.this.applic[C]
      val functor = Arrow.this.functor[C]
    }

  def <<<[A, B, C](f: F[B, C]): F[A, B] => F[A, C] =
    g => category.comp(f, g)

  def >>>[A, B, C](f: F[A, B]): F[B, C] => F[A, C] =
    g => category.comp(g, f)

  def ar[A, B](f: A => B): F[A, B] =
    arr.arr(f)

  def fst[A, B, C](f: F[A, B]): F[(A, C), (B, C)] =
    first.first(f)

  def snd[A, B, C](f: F[A, B]): F[(C, A), (C, B)] = {
    def swap[X, Y] = ar[(X, Y), (Y, X)] {
      case (x, y) => (y, x)
    }

    >>>(<<<(fst[A, B, C](f))(swap))(swap)
  }

  // ***
  def split[A, B, C, D]: F[A, B] => F[C, D] => F[(A, C), (B, D)] =
    a => c =>
      >>>(fst[A, B, C](a))(snd[C, D, B](c))

  // &&&
  def combine[A, B, C]: F[A, B] => F[A, C] => F[A, (B, C)] =
    b => c =>
      >>>(ar((a: A) => (a, a)))(split(b)(c))

  def mapfst[A, B, C](f: C => A): F[A, B] => F[C, B] =
    >>>[C, A, B](ar(f))

  def mapsnd[A, B, C](f: B => C): F[A, B] => F[A, C] =
    <<<[A, B, C](ar(f))


}

object Arrow extends Arrows

trait Arrows {
  def arrow[F[_, _]](implicit c: Category[F], a: Arr[F], f: First[F]): Arrow[F] = new Arrow[F] {
    val category = c
    val arr = a
    val first = f
  }

  implicit val Function1Arrow: Arrow[Function1] =
    arrow[Function1]

  implicit val PartialFunctionArrow: Arrow[PartialFunction] =
    arrow[PartialFunction]

  implicit def KleisliArrow[F[_]](implicit md: Monad[F]): Arrow[({type λ[α, β] = Kleisli[α, F, β]})#λ] = {
    implicit val p = md.pointed
    implicit val f = md.functor
    Arrow.arrow[({type λ[α, β] = Kleisli[α, F, β]})#λ]
  }

  implicit def CoKleisliArrow[F[_]](implicit cm: CoMonad[F]): Arrow[({type λ[α, β] = CoKleisli[α, F, β]})#λ] = {
    implicit val f = cm.coPointedFunctor
    implicit val p = cm.coPointed
    Arrow.arrow[({type λ[α, β] = CoKleisli[α, F, β]})#λ]
  }

}