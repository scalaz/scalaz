package fjs.control.parallel

import java.util.concurrent.{CompletionService, ExecutorService, Callable}
import fjs.F._
import fjs.F2._
import fjs.P1._
import fjs.Effect._
import fjs.data.List._
import fjs.data.Array._

trait Par[M[_]] {
  def parMap[B, A](f: B => A, bs: M[B])(implicit sa: fj.control.parallel.Strategy[A]): M[A]
  def parFlatMap[B, A](f: B => M[A], bs: M[B])(implicit sa: fj.control.parallel.Strategy[M[A]]): M[A]
  def parZipWith[B, C, A](f: (B, C) => A, bs: M[B], cs: M[C])(implicit sa: fj.control.parallel.Strategy[A]): M[A]
}

object Strategy {

  def par[A](a: () => A)(implicit sa: fj.control.parallel.Strategy[A]) = sa par (a)

  implicit def FF0_FP1[A](g: Function0[A] => Function0[A]): fj.F[fj.P1[A], fj.P1[A]] =
    new fj.F[fj.P1[A], fj.P1[A]]() {
      def f(p: fj.P1[A]) = g(p)
    }
  implicit def FP1_FF0[A](g: fj.F[fj.P1[A], fj.P1[A]]): Function0[A] => Function0[A] =
    (f => (() => g.f(f)._1))

  implicit def Strategy_ScalaFunction[A](s: fj.control.parallel.Strategy[A]): Function0[A] => Function0[A] = g => s.f.f(g)._1

  implicit def strategy[A](f: Function0[A] => Function0[A]): fj.control.parallel.Strategy[A] =
    fj.control.parallel.Strategy.strategy(f)

  implicit def simpleThreadStrategy[A] = fj.control.parallel.Strategy.simpleThreadStrategy[A]
  implicit def idStrategy[A] = fj.control.parallel.Strategy.idStrategy[A]
  implicit def seqStrategy[A] = fj.control.parallel.Strategy.seqStrategy[A]
  implicit def completionStrategy[A](implicit cs: CompletionService[A]): fj.control.parallel.Strategy[A] =
    fj.control.parallel.Strategy.completionStrategy(cs)
  implicit def executorStrategy[A](implicit es: ExecutorService[A]): fj.control.parallel.Strategy[A] =
    fj.control.parallel.Strategy.executorStrategy(es)
  implicit def callableStrategy[A](implicit sa: fj.control.parallel.Strategy[Callable[A]]): fj.control.parallel.Strategy[Callable[A]] =
    fj.control.parallel.Strategy.callableStrategy(sa)
  implicit def errorStrategy[A](implicit sa: fj.control.parallel.Strategy[A], e: (Error => Unit)): fj.control.parallel.Strategy[A] =
    fj.control.parallel.Strategy.errorStrategy(sa, e)

  def concurry[B, A](f: B => A)(implicit sa: fj.control.parallel.Strategy[A]): B => Function0[A] =
    sa.concurry(f) andThen {
      g => g: Function0[A]
    }

  def parMap[B, A, M[_]](f: B => A, bs: M[B])(implicit sa: fj.control.parallel.Strategy[A], m: Par[M]): M[A] =
    m.parMap(f, bs)(sa)

  def parFlatMap[B, A, M[_]]
  (f: B => M[A], bs: M[B])(implicit sa: fj.control.parallel.Strategy[M[A]], m: Par[M]): M[A] =
    m.parFlatMap(f, bs)(sa)

  def parZipWith[B, C, A, M[_]](f: (B, C) => A, bs: M[B], cs: M[C])(implicit sa: fj.control.parallel.Strategy[A],
                                                                   m: Par[M]): M[A] =
    m.parZipWith(f, bs, cs)(sa)

  implicit val ListPar: Par[List] = new Par[List] {
    def parMap[B, A](f: B => A, bs: List[B])(implicit sa: fj.control.parallel.Strategy[A]): List[A] = sa.parMap(f, bs)._1
    def parFlatMap[A, B](f: A => List[B], as: List[A])(implicit sb: fj.control.parallel.Strategy[List[B]]): List[B] =
      fj.control.parallel.Strategy.parFlatMap(sb.xmap({
        g: fj.P1[List[B]] => new fj.P1[fj.data.List[B]] {
          def _1 = g._1
        }
      }, {
        g: fj.P1[fj.data.List[B]] => new fj.P1[List[B]] {
          def _1 = g._1
        }
      }), f andThen {
        g: List[B] => g: fj.data.List[B]
      }, as)._1
    def parZipWith[B, C, A](f: (B, C) => A, bs: List[B], cs: List[C])(implicit sa: fj.control.parallel.Strategy[A]): List[A] =
      sa.parZipWith(f, bs, cs)._1
  }

  implicit val ArrayPar: Par[Array] = new Par[Array] {
    def parMap[B, A](f: B => A, bs: Array[B])(implicit sa: fj.control.parallel.Strategy[A]): Array[A] = sa.parMap(f, bs)._1
    def parFlatMap[A, B](f: A => Array[B], as: Array[A])(implicit sb: fj.control.parallel.Strategy[Array[B]]): Array[B] =
      fj.control.parallel.Strategy.parFlatMap(sb.xmap({
        g: fj.P1[Array[B]] => new fj.P1[fj.data.Array[B]] {
          def _1 = g._1
        }
      }, {
        g: fj.P1[fj.data.Array[B]] => new fj.P1[Array[B]] {
          def _1 = g._1
        }
      }), f andThen {
        g: Array[B] => g: fj.data.Array[B]
      }, as)._1
    def parZipWith[B, C, A](f: (B, C) => A, bs: Array[B], cs: Array[C])(implicit sa: fj.control.parallel.Strategy[A]): Array[A] =
      sa.parZipWith(f, bs, cs)._1
  }

}