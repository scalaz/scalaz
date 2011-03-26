package scalaz

import ST._

/** Mutable variable in state thread S containing a value of type A. */
case class STRef[S, A](a: A) {
  private var value: A = a

  /** Reads the value pointed at by this reference. */
  def read: ST[S, A] = returnST(value)

  /** Modifies the value at this reference with the given function. */
  def mod[B](f: A => A): ST[S, STRef[S, A]] = ST((s: World[S]) => {value = f(value); (s, this)})

  /** Associates this reference with the given value. */
  def write(a: A): ST[S, STRef[S, A]] = ST((s: World[S]) => {value = a; (s, this)})

  /** Swap the value at this reference with the value at another. */
  def swap(that: STRef[S, A]): ST[S, Unit] = for {
    v1 <- this.read
    v2 <- that.read
    _ <- this write v2
    _ <- that write v1
  } yield ()
}

/** Mutable array in state thread S containing values of type A. */
case class STArray[S, A:Manifest](size: Int, z: A) {
  private val value: Array[A] = Array.fill(size)(z)

  /** Reads the value at the given index. */
  def read(i: Int): ST[S, A] = returnST(value(i))

  /** Writes the given value to the array, at the given offset. */
  def write(i: Int, a: A): ST[S, STArray[S, A]] = ST(s => {value(i) = a; (s, this)})

  /** Turns a mutable array into an immutable one which is safe to return. */
  def freeze: ST[S, ImmutableArray[A]] = ST(s => (s, ImmutableArray.fromArray(value)))

  /** Fill this array from the given association list. */
  def fill[B](f: (A, B) => A, xs: Traversable[(Int, B)]): ST[S, Unit] = xs match {
    case Nil => returnST(())
    case ((i, v) :: ivs) => for {
      _ <- update(f, i, v)
      _ <- fill(f, ivs)
    } yield ()
  }

  /** Combine the given value with the value at the given index, using the given function. */
  def update[B](f: (A, B) => A, i: Int, v: B) = for {
    x <- read(i)
    _ <- write(i, f(x, v))
  } yield ()

}

/** 
 * Purely functional mutable state threads.
 * Based on JL and SPJ's paper "Lazy Functional State Threads"
 */
case class ST[S, A](f: World[S] => (World[S], A)) {
  def apply(s: World[S]) = f(s)
  def flatMap[B](g: A => ST[S, B]): ST[S, B] =
    ST(s => f(s) match { case (ns, a) => g(a)(ns) })
  def map[B](g: A => B): ST[S, B] =
    ST(s => f(s) match { case (ns, a) => (ns, g(a)) })
}

object ST {
  import Scalaz._
  import Forall._

  private[scalaz] case class World[A]()

  /** Put a value in a state thread */
  def returnST[S, A](a: => A): ST[S, A] = ST(s => (s, a))

  /** Run a state thread */
  def runST[A](f: Forall[({type λ[S] = ST[S, A]})#λ]): A =
    f.apply.f(realWorld)._2

  /** Allocates a fresh mutable reference. */
  def newVar[S, A](a: A): ST[S, STRef[S, A]] =
    returnST(STRef[S, A](a))

  /** Allocates a fresh mutable array. */
  def newArr[S, A:Manifest](size: Int, z: A): ST[S, STArray[S, A]] =
    returnST(STArray[S, A](size, z))

  /** Allows the result of a state transformer computation to be used lazily inside the computation. */
  def fixST[S, A](k: (=> A) => ST[S, A]): ST[S, A] = ST(s => {
    lazy val ans: (World[S], A) = k(r)(s)
    lazy val (_, r) = ans
    ans
  })

  /** A monoid for sequencing ST effects. */
  implicit def stMonoid[S]: Monoid[ST[S, Unit]] = new Monoid[ST[S, Unit]] {
    val zero: ST[S, Unit] = returnST(())
    def append(x: ST[S, Unit], y: => ST[S, Unit]) = x >>=| y
  }

  /** Accumulates an integer-associated list into an immutable array. */
  def accumArray[F[_]:Foldable, A: Manifest, B](size: Int, f: (A, B) => A, z: A, ivs: F[(Int, B)]): ImmutableArray[A] = { 
    type STA[S] = ST[S, ImmutableArray[A]]
    runST(Forall[STA]((k: DNE[STA]) => k(for {
      a <- newArr(size, z)
      _ <- ivs.foldMap(x => a.update(f, x._1, x._2))
      frozen <- a.freeze
    } yield frozen)))
  }

  implicit def stMonad[S]: Monad[({ type λ[A] = ST[S, A] })#λ] = new Monad[({ type λ[A] = ST[S, A] })#λ] {
    def pure[A](a: => A) = returnST(a)
    def bind[A, B](m: ST[S, A], f: A => ST[S, B]): ST[S, B] = m flatMap f
  }

  /** Equality for STRefs is reference equality */
  implicit def stRefEqual[S, A]: Equal[STRef[S, A]] = new Equal[STRef[S, A]] {
    def equal(s1: STRef[S, A], s2: STRef[S, A]): Boolean = s1 == s2
  }
    
  sealed trait RealWorld
  private val realWorld = World[RealWorld]()
  type IO[A] = ST[RealWorld, A]
}

