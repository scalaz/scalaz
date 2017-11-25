package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scalaz.Prelude._
import scalaz.data.Iso

trait Proposition[A] { A =>
  def prove(proof: ¬¬[A]): A

  def isomap[B](f: Iso[A, B]): Proposition[B] =
    p => f.to(A.prove(p.map(f.from)))

  def zip[B](implicit B: Proposition[B]): Proposition[(A, B)] =
    (proof: ¬¬[(A, B)]) => (A.prove(proof.map(_._1)), B.prove(proof.map(_._2)))
}
object Proposition {
  def apply[A](implicit A: Proposition[A]): Proposition[A] = A

  implicit def absurd[A]: Proposition[¬[A]] =
    (p: ¬¬[¬[A]]) => (a : A) => p.run(k => k(a))

  @silent implicit val voidIsProposition: Proposition[Void] = { p =>
    // Dead code warning.
    p.run(a => a)
  }

  implicit def proposition[A](implicit A: Proposition[A]): Proposition[Proposition[A]] =
    (p: ¬¬[Proposition[A]]) => A
}
