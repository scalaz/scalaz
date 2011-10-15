package scalaz
package std


trait Functions {
  implicit def function0[T] = new Traverse[Function0] with Monad[Function0] {
    def pure[A](a: => A) = () => a

    def bind[A, B](fa: () => A)(f: (A) => () => B): () => B = f(fa())

    def traverseImpl[G[_]: Applicative, A, B](fa: () => A)(f: (A) => G[B]): G[() => B] =
      Applicative[G].map(f(fa()))((b: B) => () => b)

    def foldR[A, B](fa: () => A, z: B)(f: (A) => (=> B) => B): B = f(fa())(z)
  }

  implicit def function1 = new Arr[Function1] with Category[Function1]{
    def arr[A, B](f: A => B): A => B = f

    def compose[A, B, C](f: (B) => C, g: (A) => B): (A) => C = f compose g

    def id[A]: (A) => A = a => a
  }

  implicit def function1Covariant[T] = new Monad[({type l[a] = (T => a)})#l] {
    def pure[A](a: => A) = _ => a

    def bind[A, B](fa: T => A)(f: A => T => B): (T => B) = (t: T) => f(fa(t))(t)
  }

  implicit def function1Contravariant[R] = new Contravariant[({type l[a] = (a => R)})#l] {
    def contramap[A, B](r: (A) => R)(f: (B) => A): (B) => R = null
  }

  implicit def function2[T1, T2] = new Monad[({type l[a] = ((T1, T2) => a)})#l] {
    def pure[A](a: => A): (T1, T2) => A = (t1, t2) => a

    def bind[A, B](fa: (T1, T2) => A)(f: (A) => (T1, T2) => B): (T1, T2) => B = (t1, t2) => f(fa(t1, t2))(t1, t2)
  }

  implicit def function3[T1, T2, T3] = new Monad[({type l[a] = ((T1, T2, T3) => a)})#l] {
    def pure[A](a: => A): (T1, T2, T3) => A = (t1, t2, t3) => a

    def bind[A, B](fa: (T1, T2, T3) => A)(f: (A) => (T1, T2, T3) => B): (T1, T2, T3) => B = (t1, t2, t3) => f(fa(t1, t2, t3))(t1, t2, t3)
  }
  
  implicit def function4[T1, T2, T3, T4] = new Monad[({type l[a] = ((T1, T2, T3, T4) => a)})#l] {
    def pure[A](a: => A): (T1, T2, T3, T4) => A = (t1, t2, t3, t4) => a

    def bind[A, B](fa: (T1, T2, T3, T4) => A)(f: (A) => (T1, T2, T3, T4) => B): (T1, T2, T3, T4) => B = (t1, t2, t3, t4) => f(fa(t1, t2, t3, t4))(t1, t2, t3, t4)
  }
  
  implicit def function5[T1, T2, T3, T4, T5] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5) => a)})#l] {
    def pure[A](a: => A): (T1, T2, T3, T4, T5) => A = (t1, t2, t3, t4, t5) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5) => A)(f: (A) => (T1, T2, T3, T4, T5) => B): (T1, T2, T3, T4, T5) => B = (t1, t2, t3, t4, t5) => f(fa(t1, t2, t3, t4, t5))(t1, t2, t3, t4, t5)
  }
  
  implicit def function6[T1, T2, T3, T4, T5, T6] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6) => a)})#l] {
    def pure[A](a: => A): (T1, T2, T3, T4, T5, T6) => A = (t1, t2, t3, t4, t5, t6) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6) => A)(f: (A) => (T1, T2, T3, T4, T5, T6) => B): (T1, T2, T3, T4, T5, T6) => B = (t1, t2, t3, t4, t5, t6) => f(fa(t1, t2, t3, t4, t5, t6))(t1, t2, t3, t4, t5, t6)
  }
  
  implicit def function7[T1, T2, T3, T4, T5, T6, T7] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6, T7) => a)})#l] {
    def pure[A](a: => A): (T1, T2, T3, T4, T5, T6, T7) => A = (t1, t2, t3, t4, t5, t6, t7) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7) => B): (T1, T2, T3, T4, T5, T6, T7) => B = (t1, t2, t3, t4, t5, t6, t7) => f(fa(t1, t2, t3, t4, t5, t6, t7))(t1, t2, t3, t4, t5, t6, t7)
  }
  
  implicit def function8[T1, T2, T3, T4, T5, T6, T7, T8] = new Monad[({type l[a] = ((T1, T2, T3, T4, T5, T6, T7, T8) => a)})#l] {
    def pure[A](a: => A): (T1, T2, T3, T4, T5, T6, T7, T8) => A = (t1, t2, t3, t4, t5, t6, t7, t8) => a

    def bind[A, B](fa: (T1, T2, T3, T4, T5, T6, T7, T8) => A)(f: (A) => (T1, T2, T3, T4, T5, T6, T7, T8) => B): (T1, T2, T3, T4, T5, T6, T7, T8) => B = (t1, t2, t3, t4, t5, t6, t7, t8) => f(fa(t1, t2, t3, t4, t5, t6, t7, t8))(t1, t2, t3, t4, t5, t6, t7, t8)
  }
}

object Function extends Functions
