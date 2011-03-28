package scalaz

package object effects {

  import Scalaz._
  
  private[effects] val realWorld = World[RealWorld]()

  /** Put a value in a state thread */
  def returnST[S, A](a: => A): ST[S, A] = ST(s => (s, a))

  /** Run a state thread */
  def runST[A](f: Forall[({type λ[S] = ST[S, A]})#λ]): A =
    f.apply.apply(realWorld)._2

  /** Allocates a fresh mutable reference. */
  def newVar[S, A](a: A): ST[S, STRef[S, A]] =
    returnST(new STRef[S, A](a))

  /** Allocates a fresh mutable array. */
  def newArr[S, A:Manifest](size: Int, z: A): ST[S, STArray[S, A]] =
    returnST(new STArray[S, A](size, z))

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
    runST(new Forall[STA] {
      def apply[S] = for {
        a <- newArr(size, z)
        _ <- ivs.foldMap(x => a.update(f, x._1, x._2))
        frozen <- a.freeze
      } yield frozen
    })
  }

  implicit def stMonad[S]: Monad[({ type λ[A] = ST[S, A] })#λ] = new Monad[({ type λ[A] = ST[S, A] })#λ] {
    def pure[A](a: => A) = returnST(a)
    def bind[A, B](m: ST[S, A], f: A => ST[S, B]): ST[S, B] = m flatMap f
  }

  /** Equality for STRefs is reference equality */
  implicit def stRefEqual[S, A]: Equal[STRef[S, A]] = new Equal[STRef[S, A]] {
    def equal(s1: STRef[S, A], s2: STRef[S, A]): Boolean = s1 == s2
  }

  // Implicit conversions between IO and ST
  implicit def stToIO[A](st: ST[RealWorld, A]): IO[A] = IO(st(_))
  implicit def ioToST[A](io: IO[A]): ST[RealWorld, A] = ST(io(_))
 
  // Standard I/O
  def getChar: IO[Char] = IO(rw => (rw, readChar))
  def putChar(c: Char): IO[Unit] = IO(rw => (rw, { print(c); () }))
  def putStr(s: String): IO[Unit] = IO(rw => (rw, { print(s); () }))
  def putStrLn(s: String): IO[Unit] = IO((rw => (rw, { println(s); () })))
  def readLn: IO[String] = IO(rw => (rw, readLine))
  def print[A](a: A): IO[Unit] = IO(rw => (rw, { Predef.print(a); () }))

  // Mutable variables in the IO monad
  def newIORef[A](a: => A) = stToIO(newVar(a)) >>= (v => new IORef(v).pure[IO])

  /** Throw the given error in the IO monad. */
  def throwIO[A](e: Throwable): IO[A] = IO(rw => (rw, throw e))

}

