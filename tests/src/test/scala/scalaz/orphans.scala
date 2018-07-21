// Copyright: 2017 - 2018 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import scala.AnyRef

/** instances that are defined in scalaz 7.3, but not 7.2 ... */
object orphans {
  // breaks typeclass coherence for everything above Divisible
  implicit val _decidable_equal: Decidable[Equal] = new Decidable[Equal] {
    override def divide[A1, A2, Z](a1: Equal[A1], a2: Equal[A2])(
      f: Z => (A1, A2)
    ): Equal[Z] = Equal.equal { (z1, z2) =>
      val (s1, s2) = f(z1)
      val (t1, t2) = f(z2)
      ((s1.asInstanceOf[AnyRef].eq(t1.asInstanceOf[AnyRef])) || a1
        .equal(s1, t1)) &&
      ((s2.asInstanceOf[AnyRef].eq(t2.asInstanceOf[AnyRef])) || a2
        .equal(s2, t2))
    }
    override def conquer[A]: Equal[A] = Equal.equal((_, _) => true)

    override def choose2[Z, A1, A2](a1: =>Equal[A1], a2: =>Equal[A2])(
      f: Z => A1 \/ A2
    ): Equal[Z] = Equal.equal { (z1, z2) =>
      (f(z1), f(z2)) match {
        case (-\/(s), -\/(t)) =>
          (s.asInstanceOf[AnyRef].eq(t.asInstanceOf[AnyRef])) || a1.equal(s, t)
        case (\/-(s), \/-(t)) =>
          (s.asInstanceOf[AnyRef].eq(t.asInstanceOf[AnyRef])) || a2.equal(s, t)
        case _ => false
      }
    }
  }

}
