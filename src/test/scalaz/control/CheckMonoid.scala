package scalaz.control

import MonoidLaws.{leftIdentity, rightIdentity}

object CheckMonoid {
  val props = List(leftIdentity[Option[Int]],
                   rightIdentity[Option[Int]],
                   leftIdentity[List[Int]],
                   rightIdentity[List[Int]],
                   leftIdentity[Stream[Int]],
                   rightIdentity[Stream[Int]],
                   leftIdentity[Array[Int]],
                   rightIdentity[Array[Int]])
}
