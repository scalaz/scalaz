package scalaz
package std

trait FunctionInstances1 {
  implicit def function1Semigroup[A, R](implicit R0: Semigroup[R]) = new Function1Semigroup[A, R] {
    implicit def R = R0
  }
  implicit def function1Cobind[A, R](implicit A0: Semigroup[A]): Cobind[({type λ[α]=(A => α)})#λ] = new Function1Cobind[A, R] {
    implicit def M = A0
  }
}

trait FunctionInstances0 extends FunctionInstances1 {
  implicit def function1Monoid[A, R](implicit R0: Monoid[R]) = new Function1Monoid[A, R] {
    implicit def R = R0
  }
  implicit def function1Comonad[A, R](implicit A0: Monoid[A]): Comonad[({type λ[α]=(A => α)})#λ] = new Function1Comonad[A, R] {
    implicit def M = A0
  }
}

trait FunctionInstances extends FunctionInstances0 {
  implicit val function0Instance = new Traverse[Function0] with Monad[Function0] with Comonad[Function0] with Distributive[Function0] {
    def point[A](a: => A) = () => a

    def copoint[A](p: () => A) = p()

    def cobind[A, B](fa: Function0[A])(f: Function0[A] => B) =
      () => f(fa)

    override def cojoin[A](a: Function0[A]): Function0[Function0[A]] =
      () => a

    def bind[A, B](fa: () => A)(f: (A) => () => B) = () => f(fa())()

    override def map[A,B](fa: () => A)(f: A => B) = () => f(fa())

    def traverseImpl[G[_]: Applicative, A, B](fa: () => A)(f: A => G[B]) =
      Applicative[G].map(f(fa()))((b: B) => () => b)

    override def foldRight[A, B](fa: () => A, z: => B)(f: (A, => B) => B) = f(fa(), z)

    def distributeImpl[G[_], A, B](fa: G[A])(f: A => () => B
                                 )(implicit G: Functor[G]): () => G[B] =
      () => G.map(fa)(a => f(a)())
  }

  implicit def function0Equal[R: Equal] = new Equal[() => R] {
    def equal(a1: () => R, a2: () => R) = Equal[R].equal(a1(), a2())
  }

  implicit val function1Instance = new Arrow[Function1] with Category[Function1] with Choice[Function1] {
    def arr[A, B](f: A => B) = f

    def first[A, B, C](a: A => B) =(ac: (A, C)) => (a(ac._1), ac._2)

    def compose[A, B, C](f: B => C, g: A => B) = f compose g

    def id[A]: A => A = a => a

    def choice[A, B, C](f: => A => C, g: => B => C): (A \/ B) => C = {
      case -\/(a) => f(a)
      case \/-(b) => g(b)
    }

    override def split[A, B, C, D](f: A => B, g: C => D): ((A,  C)) => (B, D) = {
      case (a, c) => (f(a), g(c))
    }

  }

  implicit def function1Covariant[T]: Monad[({type l[a] = (T => a)})#l] with Zip[({type l[a] = (T => a)})#l] with Unzip[({type l[a] = (T => a)})#l] with Distributive[({type l[a] = (T => a)})#l] = new Monad[({type l[a] = (T => a)})#l] with Zip[({type l[a] = (T => a)})#l] with Unzip[({type l[a] = (T => a)})#l] with Distributive[({type l[a] = (T => a)})#l] {
    def point[A](a: => A) = _ => a

    def bind[A, B](fa: T => A)(f: A => T => B) = (t: T) => f(fa(t))(t)

    def zip[A, B](a: => T => A, b: => T => B) =
      t => (a(t), b(t))

    def unzip[A, B](a: T => (A, B)) =
      (a(_)._1, a(_)._2)

    def distributeImpl[G[_]:Functor,A,B](fa: G[A])(f: A => T => B): T => G[B] =
      t => Functor[G].map(fa)(a => f(a)(t))

  }

  implicit def function1Contravariant[R] = new Contravariant[({type l[a] = (a => R)})#l] {
    def contramap[A, B](r: A => R)(f: B => A) = r compose f
  }

  implicit def function2Instance[T1, T2] = new Monad[({type l[a] = ((T1, T2) => a)})#l] {
    def point[A](a: => A) = (t1, t2) => a

    def bind[A, B](fa: (T1, T2) => A)(f: (A) => (T1, T2) => B) = (t1, t2) => f(fa(t1, t2))(t1, t2)
  }

  implicit def function3Instance[T1, T2, T3] = new Monad[({type l[a] = ((T1, T2, T3) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3) => a

    def bind[A, B](fa: (T1, T2, T3) => A)(f: (A) => (T1, T2, T3) => B) = (t1, t2, t3) => f(fa(t1, t2, t3))(t1, t2, t3)
  }

  implicit def function4Instance[T1, T2, T3, T4] = new Monad[({type l[a] = ((T1, T2, T3, T4) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3, t4) => a

    def bind[A, B](fa: (T1, T2, T3, T4) => A)(f: (A) => (T1, T2, T3, T4) => B) = (t1, t2, t3, t4) => f(fa(t1, t2, t3, t4))(t1, t2, t3, t4)
  }

  implicit def function5Instance[T1, T2, T3, T4, T5] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3, t4, t5) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5) => A)(f: (A) => (T1, T2, T3, T4, T5) => B) = (t1, t2, t3, t4, t5) => f(fa(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }

  implicit def function6Instance[T1, T2, T3, T4, T5, T6] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3, t4, t5, t6) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6) => A)(f: (A) => (T1, T2, T3, T4, T5, T6) => B) = (t1, t2, t3, t4, t5, t6) => f(fa(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }

  implicit def function7Instance[T1, T2, T3, T4, T5, T6, T7] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6, T7) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3, t4, t5, t6, t7) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7) => B) = (t1, t2, t3, t4, t5, t6, t7) => f(fa(t1, t2, t3, t4, t5, t6, t7))(t1, t2, t3, t4, t5, t6, t7)
  }

  implicit def function8Instance[T1, T2, T3, T4, T5, T6, T7, T8] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6, T7, T8) => a)})#l] {
    def point[A](a: => A) = (t1, t2, t3, t4, t5, t6, t7, t8) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7, T8) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7, T8) => B) = (t1, t2, t3, t4, t5, t6, t7, t8) => f(fa(t1, t2, t3, t4, t5, t6, t7, t8))(t1, t2, t3, t4, t5, t6, t7, t8)
  }
}

trait FunctionFunctions {
  /** `f(f(f(...` for referentially transparent `f`. */
  final def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }
}

object function extends FunctionFunctions with FunctionInstances

//
// Type class implementation traits
//

trait Function1Semigroup[A, R] extends Semigroup[A => R] {
  implicit def R: Semigroup[R]

  def append(f1: A => R, f2: => A => R) = a => R.append(f1(a), f2(a))
}

trait Function1Monoid[A, R] extends Monoid[A => R] with Function1Semigroup[A, R] {
  implicit def R: Monoid[R]
  def zero = a => R.zero
}

private[scalaz] trait Function1Cobind[M, R] extends Cobind[({type λ[α]=(M => α)})#λ] {
  implicit def M: Semigroup[M]
  override def cojoin[A](a: M => A) = (m1: M) => (m2: M) => a(M.append(m1, m2))
  def cobind[A, B](fa: M => A)(f: (M => A) => B) = (m1: M) => f((m2: M) => fa(M.append(m1, m2)))
  override def map[A, B](fa: M => A)(f: A => B) = fa andThen f
}

private[scalaz] trait Function1Comonad[M, R] extends Comonad[({type λ[α]=(M => α)})#λ] with Function1Cobind[M, R]{
  implicit def M: Monoid[M]
  def copoint[A](p: M => A) = p(M.zero)
}
