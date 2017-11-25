package scalaz
package data

import Prelude._
import scalaz.data.Disjunction.{-\/, \/-}
import scalaz.typeclass.Proposition

final class Inhabited[+A](val run: (A => Void) => Void) { A =>
  def map[B](f: A => B): ¬¬[B] =
    new ¬¬[B](k => A.run(a => k(f(a))))

  def flatMap[B](f: A => Inhabited[B]) =
    new ¬¬[B](k => A.run(a => f(a).run(k)))

  def zip[B](b: Inhabited[B]): ¬¬[(A, B)] =
    flatMap(a => b.flatMap(b => Inhabited.value((a, b))))

  def proved[AA >: A](implicit ev: Proposition[AA]): AA =
    ev.prove(A)
}
object Inhabited {
  def witness[A](f: (A => Void) => Void): ¬¬[A] =
    new ¬¬[A](f)

  def value[A](a: A): ¬¬[A] =
    witness(k => k(a))

  def lem[A]: ¬¬[¬[A] \/ A] =
    witness(k => k(-\/(a => k(\/-(a)))))

  def and[A, B](f: (A, B) => Void): ¬¬[¬[A] \/ ¬[B]] =
    witness(p => p(\/-(b => p(-\/(a => f(a, b))))))

  def imp[A, B](f: A => B): ¬¬[¬[A] \/ B] =
    witness(k => k(-\/(a => k(\/-(f(a))))))

  def pierce[A]: ¬¬[(¬[A] => A) => A] =
    witness(k => k((p: ¬[A] => A) => p((a: A) => k(_ => a))))

  def unsafeForce[A]: Inhabited[A] =
    witness(k => Void.unsafeForce)

  implicit def inhabitedIsProposition[A]: Proposition[Inhabited[A]] =
    (p: Inhabited[Inhabited[A]]) => p.flatMap(identity)
}
