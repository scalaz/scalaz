package scalaz

trait Semigroup[A] {
  def append(a1: A, a2: => A): A
}

object Semigroup extends Semigroups

trait Semigroups extends SemigroupsLow {
  def semigroup[A](k: A => (=> A) => A): Semigroup[A] = new Semigroup[A] {
    def append(a1: A, a2: => A) =
      k(a1)(a2)
  }

  implicit val UnitSemigroup: Semigroup[Unit] =
    semigroup(_ => _ => ())

  implicit val BooleanSemigroup: Semigroup[Boolean] =
    semigroup(a => b => a || b)

  implicit val IntSemigroup: Semigroup[Int] =
    semigroup(a => b => a + b)

  implicit val StringSemigroup: Semigroup[String] =
    semigroup(a1 => a2 => a1 + a2)

  implicit def StreamSemigroup[A]: Semigroup[Stream[A]] =
    semigroup(a1 => a2 => a1 #::: a2)

  implicit def ListSemigroup[A]: Semigroup[List[A]] =
    semigroup(a1 => a2 => a1 ::: a2)

  implicit def OptionSemigroup[A : Semigroup]: Semigroup[Option[A]] =
    semigroup(a => b =>
      (a,b) match {
        case (Some(va), Some(vb)) => Some(implicitly[Semigroup[A]].append(va, vb))
        case (Some(va), None) => Some(va)
        case (None, Some(vb)) => Some(vb)
        case (None, None) => None
      }
    )

  implicit def Tuple2Semigroup[A, B](implicit sa: Semigroup[A], sb: Semigroup[B]): Semigroup[(A, B)] =
    semigroup(a1 => a2 =>
      (sa.append(a1._1, a2._1), sb.append(a1._2, a2._2)))

  implicit def Tuple3Semigroup[A, B, C](implicit sa: Semigroup[A], sb: Semigroup[B], sc: Semigroup[C]): Semigroup[(A, B, C)] =
    semigroup(a1 => a2 =>
      (sa.append(a1._1, a2._1), sb.append(a1._2, a2._2), sc.append(a1._3, a2._3)))
}

trait SemigroupsLow {
  implicit def TraversableSemigroup[X, CC[Y] <: collection.TraversableLike[Y, CC[Y]] : CanBuildAnySelf]: Semigroup[CC[X]] = new Semigroup[CC[X]] {
    def append(s1: CC[X], s2: => CC[X]): CC[X] = {
      implicit val cbf = implicitly[CanBuildAnySelf[CC]].builder[X, X]
      s1 ++ s2
    }
  }
}