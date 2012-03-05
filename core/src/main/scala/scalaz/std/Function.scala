package scalaz
package std

trait FunctionInstances1 {
  implicit def function1Semigroup[A, R](implicit R0: Semigroup[R]) = new Function1Semigroup[A, R] {
    implicit def R = R0
  }
}

trait FunctionInstances0 extends FunctionInstances1 {
  implicit def function1Monoid[A, R](implicit R0: Monoid[R]) = new Function1Monoid[A, R] {
    implicit def R = R0
  }
  implicit def function1CoMonad[A, R](implicit A0: Monoid[A]) = new Function1CoMonad[A, R] {
    implicit def M = A0
  }
}

trait FunctionInstances extends FunctionInstances0 {
  implicit def function0Instance[T] = new Traverse[Function0] with Monad[Function0] with CoPointed[Function0] {
    def point[A](a: => A) = () => a

    def copoint[A](p: () => A) = p()

    def bind[A, B](fa: () => A)(f: (A) => () => B) = f(fa())

    def traverseImpl[G[_]: Applicative, A, B](fa: () => A)(f: (A) => G[B]) =
      Applicative[G].map(f(fa()))((b: B) => () => b)

    override def foldRight[A, B](fa: () => A, z: => B)(f: (A, => B) => B) = f(fa(), z)
  }

  implicit def function0Equal[R: Equal] = new Equal[() => R] {
    def equal(a1: () => R, a2: () => R) = Equal[R].equal(a1(), a2())
  }

  implicit def function1Instance = new Arrow[Function1] with Category[Function1]{
    def arr[A, B](f: A => B) = f

    def first[A, B, C](a: A => B) =(ac: (A, C)) => (a(ac._1), ac._2)
    
    def compose[A, B, C](f: (B) => C, g: (A) => B) = f compose g

    def id[A]: (A) => A = a => a
  }

  implicit def function1Covariant[T]: Monad[({type l[a] = (T => a)})#l] = new Monad[({type l[a] = (T => a)})#l] {
    def point[A](a: => A) = _ => a

    def bind[A, B](fa: T => A)(f: A => T => B) = (t: T) => f(fa(t))(t)
  }

  implicit def function1Contravariant[R] = new Contravariant[({type l[a] = (a => R)})#l] {
    def contramap[A, B](r: (A) => R)(f: (B) => A) = null
  }

  implicit def function1Group[A, R](implicit R0: Group[R]) = new Function1Group[A, R] {
    implicit def R: Group[R] = R0
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

object function extends FunctionInstances

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

trait Function1CoMonad[M, R] extends CoMonad[({type λ[α]=(M => α)})#λ] {
  implicit def M: Monoid[M]
  def cojoin[A](a: (M) => A) = (m1: M) => (m2: M) => a(M.append(m1, m1))
  def copoint[A](p: (M) => A) = p(M.zero)
  def cobind[A, B](fa: (M) => A)(f: ((M) => A) => B) = (m1: M) => f((m2: M) => fa(M.append(m1, m2)))
  def map[A, B](fa: (M) => A)(f: (A) => B) = fa andThen f
}

trait Function1Group[A, R] extends Group[A => R] with Function1Monoid[A, R] {
  implicit def R: Group[R]
  def inverse(f: A=>R) = a => R.inverse(f(a))
}
