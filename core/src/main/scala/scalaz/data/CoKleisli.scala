package scalaz

trait Cokleisli[W[_], A, B] {
  def apply(a: W[A]): B

  import Scalaz._

  def <<=(a: W[A])(implicit w: Comonad[W]): W[B] = a =>> apply

  def =>=[C](c: Cokleisli[W, B, C])(implicit b: Comonad[W]): Cokleisli[W, A, C] = ★(e => c(e =>> (Cokleisli.this(_))))

  def =>=[C](c: W[B] => C)(implicit b: Comonad[W]): Cokleisli[W, A, C] = =>=(★(c))

  def =<=[C](c: Cokleisli[W, C, A])(implicit b: Comonad[W]): Cokleisli[W, C, B] = c =>= this

  def =<=[C](c: W[C] => A)(implicit b: Comonad[W]): Cokleisli[W, C, B] = ★(c) =>= this
}

trait Cokleislis {
  def ★[W[_], A, B](f: W[A] => B): Cokleisli[W, A, B] = new Cokleisli[W, A, B] {
    def apply(a: W[A]) = f(a)
  }

  def cokleisli[W[_], A, B](f: W[A] => B): Cokleisli[W, A, B] = ★(f)
}
