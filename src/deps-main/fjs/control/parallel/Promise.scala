package fjs.control.parallel

import fjs.P1._
import fjs.Unit._
import fjs.F2._
import fjs.F._
import fjs.data.List._

object Promise {
  
  def promise[A](p: () => A)(implicit s: fj.control.parallel.Strategy[fj.Unit]) =
    fj.control.parallel.Promise.promise(s, p)

  def promise[A](p: java.util.concurrent.Callable[A])(implicit s: fj.control.parallel.Strategy[fj.Unit]) =
    fj.control.parallel.Promise.promise(s, p)

  def promise[A, B](f: A => B)(implicit s: fj.control.parallel.Strategy[fj.Unit]) =
    fj.control.parallel.Promise.promise(s, f)

  def join[A](p: () => fj.control.parallel.Promise[A])(implicit s: fj.control.parallel.Strategy[fj.Unit]) =
    fj.control.parallel.Promise.join(s, p)

  def sequence[A](ps: List[fj.control.parallel.Promise[A]])(implicit s: fj.control.parallel.Strategy[fj.Unit]): fj.control.parallel.Promise[List[A]] =
    fj.control.parallel.Promise.sequence(s, ps) fmap {g: fj.data.List[A] => g: List[A]}

  def foldRight[A, B](g: A => B => B, b: B, as: List[A])(implicit s: fj.control.parallel.Strategy[fj.Unit]): fj.control.parallel.Promise[B] =
    fj.control.parallel.Promise.foldRight[A, B](s, new fj.F[A, fj.F[B, B]]{ def f(a: A) = g(a) }, b).f(as)

}
