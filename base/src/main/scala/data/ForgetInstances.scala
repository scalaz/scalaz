package scalaz
package data
import typeclass.{Profunctor, Strong}

trait ForgetInstances { self =>
  implicit def profunctor[A]: Profunctor[Forget[A, ?, ?]] = new Profunctor[Forget[A, ?, ?]] {
    override def dimap[B, C, D, E](fbc: Forget[A, B, C])(fdb: D => B)(fce: C => E): Forget[A, D, E] = 
      Forget[A, D, E](fdb andThen fbc.forget) 

    override def lmap[B, C, D](fbc: Forget[A, B, C])(fdb: D => B): Forget[A, D, C] =
      Forget[A, D, C](fdb andThen fbc.forget)
      
    override def rmap[B, C, D](fbc: Forget[A, B, C])(fcd: C => D): Forget[A, B, D] =
      fbc.retag[D]
  }

 implicit def strong[A]: Strong[Forget[A, ?, ?]] = new Strong[Forget[A, ?, ?]] {
    override def profunctor: Profunctor[Forget[A, ?, ?]] = self.profunctor[A]
 }
}

