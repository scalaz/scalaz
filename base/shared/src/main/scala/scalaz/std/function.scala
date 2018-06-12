package scalaz
package std

import scala.{ Function0, Function1 }
import scalaz.ct._
import scalaz.data.Disjunction

trait Function0Instances {

  implicit val function0Monad: Monad[Function0] = instanceOf(
    new MonadClass[Function0] with BindClass.DeriveFlatten[Function0] {
      override def ap[A, B](fab: Function0[A])(f: Function0[A => B]): Function0[B]      = () => f()(fab())
      override def map[A, B](fab: Function0[A])(f: A => B): Function0[B]                = () => f(fab())
      override def flatMap[A, B](fab: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(fab())()
      override def pure[A](a: A): Function0[A]                                          = () => a
    }
  )

  implicit val function0Cobind: Comonad[Function0] = instanceOf(
    new ComonadClass[Function0] with CobindClass.DeriveCojoin[Function0] {
      override def map[A, B](fa: Function0[A])(f: A => B): Function0[B] = () => f(fa())

      override def cobind[A, B](fa: Function0[A])(f: Function0[A] => B): Function0[B] =
        () => f(fa)

      override def copoint[A](fa: Function0[A]): A = fa()
    }
  )

}

trait Function1Instances {

  implicit def function1Monad[C]: Monad[Function1[C, ?]] =
    instanceOf(new MonadClass[Function1[C, ?]] with BindClass.DeriveFlatten[Function1[C, ?]] {
      override def ap[A, B](fab: Function1[C, A])(f: Function1[C, A => B]): Function1[C, B] = (c: C) => f(c)(fab(c))
      override def map[A, B](fab: Function1[C, A])(f: A => B): Function1[C, B]              = fab andThen f
      override def flatMap[A, B](fab: Function1[C, A])(f: A => Function1[C, B]): Function1[C, B] =
        (c: C) => f(fab(c))(c)
      override def pure[A](a: A): Function1[C, A] = (c: C) => a
    })

  trait Function1Profunctor extends ProfunctorClass.DeriveDimap[? => ?] {
    override def lmap[A, B, C](fab: A => B)(ca: C => A): C => B =
      fab compose ca
    override def rmap[A, B, C](fab: A => B)(bc: B => C): A => C =
      fab andThen bc
  }

  implicit val function1Choice: Choice[? => ?] = instanceOf(
    new ChoiceClass[? => ?] with Function1Profunctor {

      override def leftchoice[A, B, C](ab: A => B): A \/ C => B \/ C =
        _.fold(a => Disjunction.left(ab(a)), Disjunction.right)

      override def rightchoice[A, B, C](ab: A => B): C \/ A => C \/ B =
        _.fold(Disjunction.left, a => Disjunction.right(ab(a)))
    }
  )

  implicit val function1Strong: Strong[? => ?] = instanceOf(
    new StrongClass[? => ?] with Function1Profunctor {

      override def first[A, B, C](pab: A => B): ((A, C)) => (B, C) = {
        case (a, c) => (pab(a), c)
      }

      override def second[A, B, C](pab: A => B): ((C, A)) => (C, B) = {
        case (c, a) => (c, pab(a))
      }
    }
  )

}
