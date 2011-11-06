package scalaz
package syntax
package std

trait Function2V[T1, T2, R] extends SyntaxV[(T1, T2) => R] {
  def flip: (T2, T1) => R = (v2: T2, v1: T1) => self(v1, v2)

  def on[X](f: (R, R) => X, t1: (T1, T1), t2: (T2, T2)): X = f(self(t1._1, t2._1), self(t1._2, t2._2))

  def contramap[TT](f: TT => T1)(implicit ev: T1 =:= T2): (TT, TT) => R = (t1, t2) => self(f(t1), ev(f(t2)))

  def lift[F[_]](implicit F: Applicative[F]): (F[T1], F[T2]) => F[R] = F.lift2(self)

  def byName: (=> T1, => T2) => R = (t1, t2) => self(t1, t2)
}

trait ToFunction2V {
  implicit def ToFunction2V[T1, T2, R](f: (T1, T2) => R) = new Function2V[T1, T2, R] {
    val self = f
  }
}