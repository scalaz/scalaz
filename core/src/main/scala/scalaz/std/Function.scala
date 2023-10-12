package scalaz
package std

sealed trait FunctionInstances1 {
  implicit def function1Semigroup[A, R](implicit R0: Semigroup[R]): Semigroup[A => R] =
    new Function1Semigroup[A, R] {
      override def R = R0
    }
  implicit def function1Cobind[A, R](implicit A0: Semigroup[A]): Cobind[A => *] =
    new Function1Cobind[A, R] {
      override def M = A0
    }
}

sealed trait FunctionInstances0 extends FunctionInstances1 {
  implicit def function1Monoid[A, R](implicit R0: Monoid[R]): Monoid[A => R] =
    new Function1Monoid[A, R] {
      override def R = R0
    }
  implicit def function1Comonad[A, R](implicit A0: Monoid[A]): Comonad[A => *] =
    new Function1Comonad[A, R] {
      override def M = A0
    }
  // See SI-7899. Scala 2.11 will no longer infer by-name types for type parameter `T` (which was unsound.)
  // Scala doesn't support abstraction over by-name-ness.
  // In Scala 2.11.0-M5 and below, a few places in `Free.scala` inferred the implicit
  // `function1Covariant[=> Any]`. In 2.11.0-M6 and above, they would infer this value.
  // Those places have been change to explicitly use this instance so that we don't see different
  // behaviour based on Scala version.
  implicit def function1CovariantByName[T]: Monad[(=> T) => *] with BindRec[(=> T) => *] with Zip[(=> T) => *] with Unzip[(=> T) => *] with Distributive[(=> T) => *] =
    new Monad[(=> T) => *] with BindRec[(=> T) => *] with Zip[(=> T) => *] with Unzip[(=> T) => *] with Distributive[(=> T) => *] {
      def point[A](a: => A): (=> T) => A = _ => a

      def bind[A, B](fa: (=> T) => A)(f: A => (=> T) => B): (=> T) => B = (t) => f(fa(t))(t)

      def zip[A, B](a: => (=> T) => A, b: => (=> T) => B): (=> T) => (A, B) =
        t => (a(t), b(t))

      def unzip[A, B](a: (=> T) => (A, B)): ((=> T) => A, (=> T) => B) =
        (a(_)._1, a(_)._2)

      def distributeImpl[G[_]: Functor, A, B](fa: G[A])(f: A => (=> T) => B): (=> T) => G[B] =
        t => Functor[G].map(fa)(a => f(a)(t))

      def tailrecM[A, B](f: A => (=> T) => A \/ B)(a: A): (=> T) => B =
        t => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }
}

trait FunctionInstances extends FunctionInstances0 {
  implicit val function0Instance: Traverse[Function0] with Monad[Function0] with BindRec[Function0] with Comonad[Function0] with Distributive[Function0] =
    new Traverse[Function0] with Monad[Function0] with BindRec[Function0] with Comonad[Function0] with Distributive[Function0] {
      def point[A](a: => A) =
        () => a

      def copoint[A](p: () => A) =
        p()

      def cobind[A, B](fa: Function0[A])(f: Function0[A] => B) =
        () => f(fa)

      override def cojoin[A](a: Function0[A]): Function0[Function0[A]] =
        () => a

      def bind[A, B](fa: () => A)(f: (A) => () => B) =
        () => f(fa())()

      override def map[A, B](fa: () => A)(f: A => B) =
        () => f(fa())

      def traverseImpl[G[_]: Applicative, A, B](fa: () => A)(f: A => G[B]) =
        Applicative[G].map(f(fa()))((b: B) => () => b)

      override def foldRight[A, B](fa: () => A, z: => B)(f: (A, => B) => B) =
        f(fa(), z)

      def distributeImpl[G[_], A, B](fa: G[A])(f: A => () => B)(implicit G: Functor[G]): () => G[B] =
        () => G.map(fa)(a => f(a)())

      def tailrecM[A, B](f: A => () => A \/ B)(a: A): () => B =
        () => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)() match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function0Equal[R: Equal]: Equal[() => R] =
    new Equal[() => R] {
      def equal(a1: () => R, a2: () => R) = Equal[R].equal(a1(), a2())
    }

  implicit val function1Instance: Arrow[Function1] with Choice[Function1] with ProChoice[Function1] =
    new Arrow[Function1] with Choice[Function1] with ProChoice[Function1] {
      def left[A, B, C](fa: A => B) = _.leftMap(fa)

      def right[A, B, C](fa: A => B) = _.map(fa)

      def arr[A, B](f: A => B) = f

      def first[A, B, C](a: A => B) = (ac: (A, C)) => (a(ac._1), ac._2)

      def compose[A, B, C](f: B => C, g: A => B) = f compose g

      def id[A]: A => A = a => a

      def choice[A, B, C](f: => A => C, g: => B => C): (A \/ B) => C = {
        case -\/(a) => f(a)
        case \/-(b) => g(b)
      }

      override def split[A, B, C, D](f: A => B, g: C => D): ((A, C)) => (B, D) = {
        case (a, c) => (f(a), g(c))
      }
    }

  implicit def function1Covariant[T]: Monad[T => *] with BindRec[T => *] with Zip[T => *] with Unzip[T => *] with Distributive[T => *] =
    new Monad[T => *] with BindRec[T => *] with Zip[T => *] with Unzip[T => *] with Distributive[T => *] {
      def point[A](a: => A) =
        _ => a

      def bind[A, B](fa: T => A)(f: A => T => B) =
        t => f(fa(t))(t)

      def zip[A, B](a: => T => A, b: => T => B) =
        t => (a(t), b(t))

      def unzip[A, B](a: T => (A, B)) =
        (a(_)._1, a(_)._2)

      def distributeImpl[G[_]: Functor, A, B](fa: G[A])(f: A => T => B): T => G[B] =
        t => Functor[G].map(fa)(a => f(a)(t))

      def tailrecM[A, B](f: A => T => A \/ B)(a: A): T => B =
        t => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function1Contravariant[R]: Contravariant[* => R] =
    new Contravariant[* => R] {
      def contramap[A, B](r: A => R)(f: B => A) = r compose f
    }

  implicit def function2Instance[T1, T2]: Monad[(T1, T2) => *] with BindRec[(T1, T2) => *] =
    new Monad[(T1, T2) => *] with BindRec[(T1, T2) => *] {
      def point[A](a: => A) =
        (_, _) => a

      def bind[A, B](fa: (T1, T2) => A)(f: (A) => (T1, T2) => B) =
        (t1, t2) => f(fa(t1, t2))(t1, t2)

      def tailrecM[A, B](f: A => (T1, T2) => A \/ B)(a: A): (T1, T2) => B =
        (t1, t2) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function3Instance[T1, T2, T3]: Monad[(T1, T2, T3) => *] with BindRec[(T1, T2, T3) => *] =
    new Monad[(T1, T2, T3) => *] with BindRec[(T1, T2, T3) => *] {
      def point[A](a: => A) =
        (_, _, _) => a

      def bind[A, B](fa: (T1, T2, T3) => A)(f: (A) => (T1, T2, T3) => B) =
        (t1, t2, t3) => f(fa(t1, t2, t3))(t1, t2, t3)

      def tailrecM[A, B](f: A => (T1, T2, T3) => A \/ B)(a: A): (T1, T2, T3) => B =
        (t1, t2, t3) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2, t3) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function4Instance[T1, T2, T3, T4]: Monad[(T1, T2, T3, T4) => *] with BindRec[(T1, T2, T3, T4) => *] =
    new Monad[(T1, T2, T3, T4) => *] with BindRec[(T1, T2, T3, T4) => *] {
      def point[A](a: => A) =
        (_, _, _, _) => a

      def bind[A, B](fa: (T1, T2, T3, T4) => A)(f: (A) => (T1, T2, T3, T4) => B) =
        (t1, t2, t3, t4) => f(fa(t1, t2, t3, t4))(t1, t2, t3, t4)

      def tailrecM[A, B](f: A => (T1, T2, T3, T4) => A \/ B)(a: A): (T1, T2, T3, T4) => B =
        (t1, t2, t3, t4) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2, t3, t4) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function5Instance[T1, T2, T3, T4, T5]: Monad[(T1, T2, T3, T4, T5) => *] with BindRec[(T1, T2, T3, T4, T5) => *] =
    new Monad[(T1, T2, T3, T4, T5) => *] with BindRec[(T1, T2, T3, T4, T5) => *] {
      def point[A](a: => A) =
        (_, _, _, _, _) => a

      def bind[A, B](fa: (T1, T2, T3, T4, T5) => A)(f: (A) => (T1, T2, T3, T4, T5) => B) =
        (t1, t2, t3, t4, t5) => f(fa(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)

      def tailrecM[A, B](f: A => (T1, T2, T3, T4, T5) => A \/ B)(a: A): (T1, T2, T3, T4, T5) => B =
        (t1, t2, t3, t4, t5) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2, t3, t4, t5) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function6Instance[T1, T2, T3, T4, T5, T6]: Monad[(T1, T2, T3, T4, T5, T6) => *] with BindRec[(T1, T2, T3, T4, T5, T6) => *] =
    new Monad[(T1, T2, T3, T4, T5, T6) => *] with BindRec[(T1, T2, T3, T4, T5, T6) => *] {
      def point[A](a: => A) =
        (_, _, _, _, _, _) => a

      def bind[A, B](fa: (T1, T2, T3, T4, T5, T6) => A)(f: (A) => (T1, T2, T3, T4, T5, T6) => B) =
        (t1, t2, t3, t4, t5, t6) => f(fa(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)

      def tailrecM[A, B](f: A => (T1, T2, T3, T4, T5, T6) => A \/ B)(a: A): (T1, T2, T3, T4, T5, T6) => B =
        (t1, t2, t3, t4, t5, t6) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2, t3, t4, t5, t6) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function7Instance[T1, T2, T3, T4, T5, T6, T7]: Monad[(T1, T2, T3, T4, T5, T6, T7) => *] with BindRec[(T1, T2, T3, T4, T5, T6, T7) => *] =
    new Monad[(T1, T2, T3, T4, T5, T6, T7) => *] with BindRec[(T1, T2, T3, T4, T5, T6, T7) => *] {
      def point[A](a: => A) =
        (_, _, _, _, _, _, _) => a

      def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7) => B) =
        (t1, t2, t3, t4, t5, t6, t7) => f(fa(t1, t2, t3, t4, t5, t6, t7))(t1, t2, t3, t4, t5, t6, t7)

      def tailrecM[A, B](f: A => (T1, T2, T3, T4, T5, T6, T7) => A \/ B)(a: A): (T1, T2, T3, T4, T5, T6, T7) => B =
        (t1, t2, t3, t4, t5, t6, t7) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2, t3, t4, t5, t6, t7) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }

  implicit def function8Instance[T1, T2, T3, T4, T5, T6, T7, T8]: Monad[(T1, T2, T3, T4, T5, T6, T7, T8) => *] with BindRec[(T1, T2, T3, T4, T5, T6, T7, T8) => *] =
    new Monad[(T1, T2, T3, T4, T5, T6, T7, T8) => *] with BindRec[(T1, T2, T3, T4, T5, T6, T7, T8) => *] {
      def point[A](a: => A) =
        (_, _, _, _, _, _, _, _) => a

      def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7, T8) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7, T8) => B) =
        (t1, t2, t3, t4, t5, t6, t7, t8) => f(fa(t1, t2, t3, t4, t5, t6, t7, t8))(t1, t2, t3, t4, t5, t6, t7, t8)

      def tailrecM[A, B](f: A => (T1, T2, T3, T4, T5, T6, T7, T8) => A \/ B)(a: A): (T1, T2, T3, T4, T5, T6, T7, T8) => B =
        (t1, t2, t3, t4, t5, t6, t7, t8) => {
          @scala.annotation.tailrec
          def go(a0: A): B =
            f(a0)(t1, t2, t3, t4, t5, t6, t7, t8) match {
              case \/-(b) => b
              case -\/(a1) => go(a1)
            }
          go(a)
        }
    }
}

trait FunctionFunctions {
  /**
   * `f(f(f(...` for referentially transparent `f`.
   * @since 7.0.1
   */
  final def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }
}

object function extends FunctionFunctions with FunctionInstances

//
// Type class implementation traits
//

private trait Function1Semigroup[A, R] extends Semigroup[A => R] {
  implicit def R: Semigroup[R]

  def append(f1: A => R, f2: => A => R) = a => R.append(f1(a), f2(a))
}

private trait Function1Monoid[A, R] extends Monoid[A => R] with Function1Semigroup[A, R] {
  implicit def R: Monoid[R]
  def zero = a => R.zero
}

private trait Function1Cobind[M, R] extends Cobind[M => *] {
  implicit def M: Semigroup[M]
  override def cojoin[A](a: M => A) = (m1: M) => (m2: M) => a(M.append(m1, m2))
  def cobind[A, B](fa: M => A)(f: (M => A) => B) = (m1: M) => f((m2: M) => fa(M.append(m1, m2)))
  override def map[A, B](fa: M => A)(f: A => B) = fa andThen f
}

private trait Function1Comonad[M, R] extends Comonad[M => *] with Function1Cobind[M, R] {
  implicit def M: Monoid[M]
  def copoint[A](p: M => A) = p(M.zero)
}
