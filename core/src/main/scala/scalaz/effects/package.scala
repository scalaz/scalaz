package scalaz

import java.io._

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

  implicit def stMonoid[S, A: Monoid]: Monoid[ST[S, A]] = Monoid.liftMonoid[({ type λ[A] = ST[S, A] })#λ, A]
  implicit def ioMonoid[A: Monoid]: Monoid[IO[A]] = Monoid.liftMonoid

  implicit def stApplicative[S]: Applicative[({ type λ[A] = ST[S, A] })#λ] = stMonad[S]

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
 
  /** Perform the given side-effect in an IO action */
  def io[A](a: => A) = a.pure[IO]

  /** Get the next character from standard input */
  def getChar: IO[Char] = IO(rw => (rw, readChar))

  /** Write a character to standard output */
  def putChar(c: Char): IO[Unit] = IO(rw => (rw, { print(c); () }))

  /** Write a String to standard output */
  def putStr(s: String): IO[Unit] = IO(rw => (rw, { print(s); () }))

  /** Write a String to standard output, followed by a newline */
  def putStrLn(s: String): IO[Unit] = IO((rw => (rw, { println(s); () })))

  /** Read the next line from standard input */
  def readLn: IO[String] = IO(rw => (rw, readLine))

  /** Print the given object to standard output */
  def putOut[A](a: A): IO[Unit] = IO(rw => (rw, { print(a); () }))

  import IterV._
  import java.io._

  /** Repeatedly apply the given action, enumerating the results. */
  def enumerateBy[A](action: IO[A]): EnumeratorM[IO, A] = new EnumeratorM[IO, A] {
    def apply[B](i: IterV[A, B]): IO[IterV[A, B]] = i match {
      case Done(a, s) => io { Done(a, s) }
      case Cont(k) => action map (a => k(El(a)))
    }
  }

  /** Enumerate the lines on standard input as Strings */
  val getLines: EnumeratorM[IO, String] = getReaderLines(new BufferedReader(new InputStreamReader(System.in)))

  /** Read a line from a buffered reader */
  def rReadLn(r: BufferedReader): IO[Option[String]] = io { Option(r.readLine) }

  /** Write a string to a PrintWriter */
  def wPutStr(w: PrintWriter, s: String): IO[Unit] = io { w.print(s) }

  /** Write a String to a PrintWriter, followed by a newline */
  def wPutStrLn(w: PrintWriter, s: String): IO[Unit] = io { w.println(s) }

  /** Enumerate the lines from a BufferedReader */
  def getReaderLines(r: => BufferedReader): EnumeratorM[IO, String] = new EnumeratorM[IO, String] {
    def apply[A](it: IterV[String, A]) = {
      def loop: IterV[String, A] => IO[IterV[String, A]] = {
        case i@Done(_, _) => io { i }
        case i@Cont(k) => for {
          s <- rReadLn(r)
          a <- s.map(l => loop(k(El(l)))).getOrElse(io(i))
        } yield a
      }
      loop(it)
    }
  }
  def closeReader(r: Reader): IO[Unit] = io { r.close }

  def getFileLines(f: File): EnumeratorM[IO, String] = new EnumeratorM[IO, String] {
    def apply[A](i: IterV[String, A]) = bufferFile(f).bracket(closeReader)(getReaderLines(_)(i))
  }

  def bufferFile(f: File): IO[BufferedReader] = io { new BufferedReader(new FileReader(f)) }

  // Mutable variables in the IO monad
  def newIORef[A](a: => A) = stToIO(newVar(a)) >>= (v => io { new IORef(v) })

  /** Throw the given error in the IO monad. */
  def throwIO[A](e: Throwable): IO[A] = IO(rw => (rw, throw e))

}

