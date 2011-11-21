package scalaz
package std

trait FunctionInstances0 {
  implicit def function1Semigroup[A, R](implicit R0: Semigroup[R]) = new Function1Semigroup[A, R] {
    implicit def R: Semigroup[R] = R0
  }
}

trait FunctionInstances extends FunctionInstances0 {
  implicit def function0Instance[T] = new Traverse[Function0] with Monad[Function0] {
    def point[A](a: => A) = () => a

    def bind[A, B](fa: () => A)(f: (A) => () => B): () => B = f(fa())

    def traverseImpl[G[_]: Applicative, A, B](fa: () => A)(f: (A) => G[B]): G[() => B] =
      Applicative[G].map(f(fa()))((b: B) => () => b)

    def foldRight[A, B](fa: () => A, z: => B)(f: (A, => B) => B): B = f(fa(), z)
  }

  implicit def function0Equal[R: Equal] = new Equal[() => R] {
    def equal(a1: () => R, a2: () => R) = Equal[R].equal(a1(), a2())
  }

  implicit def function1Instance = new Arrow[Function1] with Arr[Function1] with Category[Function1]{
    def arr[A, B](f: A => B): A => B = f

    def first[A, B, C](a: A => B) =(ac: (A, C)) => (a(ac._1), ac._2)
    
    def compose[A, B, C](f: (B) => C, g: (A) => B): (A) => C = f compose g

    def id[A]: (A) => A = a => a
  }

  implicit def function1Covariant[T] = new Monad[({type l[a] = (T => a)})#l] {
    def point[A](a: => A) = _ => a

    def bind[A, B](fa: T => A)(f: A => T => B): (T => B) = (t: T) => f(fa(t))(t)
  }

  implicit def function1Contravariant[R] = new Contravariant[({type l[a] = (a => R)})#l] {
    def contramap[A, B](r: (A) => R)(f: (B) => A): (B) => R = null
  }
  
  implicit def function1Monoid[A, R](implicit R0: Monoid[R]) = new Function1Monoid[A, R] {
    implicit def R: Monoid[R] = R0
  }

  implicit def function1Group[A, R](implicit R0: Group[R]) = new Function1Group[A, R] {
    implicit def R: Group[R] = R0
  }

  implicit def function2Instance[T1, T2] = new Monad[({type l[a] = ((T1, T2) => a)})#l] {
    def point[A](a: => A): (T1, T2) => A = (t1, t2) => a

    def bind[A, B](fa: (T1, T2) => A)(f: (A) => (T1, T2) => B): (T1, T2) => B = (t1, t2) => f(fa(t1, t2))(t1, t2)
  }

  implicit def function3Instance[T1, T2, T3] = new Monad[({type l[a] = ((T1, T2, T3) => a)})#l] {
    def point[A](a: => A): (T1, T2, T3) => A = (t1, t2, t3) => a

    def bind[A, B](fa: (T1, T2, T3) => A)(f: (A) => (T1, T2, T3) => B): (T1, T2, T3) => B = (t1, t2, t3) => f(fa(t1, t2, t3))(t1, t2, t3)
  }
  
  implicit def function4Instance[T1, T2, T3, T4] = new Monad[({type l[a] = ((T1, T2, T3, T4) => a)})#l] {
    def point[A](a: => A): (T1, T2, T3, T4) => A = (t1, t2, t3, t4) => a

    def bind[A, B](fa: (T1, T2, T3, T4) => A)(f: (A) => (T1, T2, T3, T4) => B): (T1, T2, T3, T4) => B = (t1, t2, t3, t4) => f(fa(t1, t2, t3, t4))(t1, t2, t3, t4)
  }
  
  implicit def function5Instance[T1, T2, T3, T4, T5] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5) => a)})#l] {
    def point[A](a: => A): (T1, T2, T3, T4, T5) => A = (t1, t2, t3, t4, t5) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5) => A)(f: (A) => (T1, T2, T3, T4, T5) => B): (T1, T2, T3, T4, T5) => B = (t1, t2, t3, t4, t5) => f(fa(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }
  
  implicit def function6Instance[T1, T2, T3, T4, T5, T6] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6) => a)})#l] {
    def point[A](a: => A): (T1, T2, T3, T4, T5, T6) => A = (t1, t2, t3, t4, t5, t6) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6) => A)(f: (A) => (T1, T2, T3, T4, T5, T6) => B): (T1, T2, T3, T4, T5, T6) => B = (t1, t2, t3, t4, t5, t6) => f(fa(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }
  
  implicit def function7Instance[T1, T2, T3, T4, T5, T6, T7] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6, T7) => a)})#l] {
    def point[A](a: => A): (T1, T2, T3, T4, T5, T6, T7) => A = (t1, t2, t3, t4, t5, t6, t7) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7) => B): (T1, T2, T3, T4, T5, T6, T7) => B = (t1, t2, t3, t4, t5, t6, t7) => f(fa(t1, t2, t3, t4, t5, t6, t7))(t1, t2, t3, t4, t5, t6, t7)
  }
  
  implicit def function8Instance[T1, T2, T3, T4, T5, T6, T7, T8] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6, T7, T8) => a)})#l] {
    def point[A](a: => A): (T1, T2, T3, T4, T5, T6, T7, T8) => A = (t1, t2, t3, t4, t5, t6, t7, t8) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7, T8) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7, T8) => B): (T1, T2, T3, T4, T5, T6, T7, T8) => B = (t1, t2, t3, t4, t5, t6, t7, t8) => f(fa(t1, t2, t3, t4, t5, t6, t7, t8))(t1, t2, t3, t4, t5, t6, t7, t8)
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

trait Function1Group[A, R] extends Group[A => R] with Function1Monoid[A, R] {
  implicit def R: Group[R]
  def inverse(f: A=>R) = a => R.inverse(f(a))
}
