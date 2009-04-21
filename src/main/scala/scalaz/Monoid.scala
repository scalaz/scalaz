package scalaz

trait Monoid[M] extends Zero[M] with Semigroup[M]

object Monoid {
  def monoid[M](implicit s: Semigroup[M], z: Zero[M]) = new Monoid[M] {
    def append(s1: M, s2: => M) = s append (s1, s2)
    val zero = z.zero
  }

  implicit val DigitMonoid = monoid[Digit]
  
  implicit val OrderingMonoid = monoid[Ordering]

  implicit val UnitMonoid = monoid[Unit]

  implicit val IntMonoid = monoid[Int]

  implicit val IntMultiplicationMonoid = monoid[IntMultiplication]

  implicit val BooleanConjunctionMonoid = monoid[BooleanConjunction]

  implicit val BooleanMonoid = monoid[Boolean]

  implicit val CharMonoid = monoid[Char]

  implicit val CharMultiplicationMonoid = monoid[CharMultiplication]

  implicit val ByteMonoid = monoid[Byte]

  implicit val ByteMultiplicationMonoid = monoid[ByteMultiplication]

  implicit val LongMonoid = monoid[Long]

  implicit val LongMultiplicationMonoid = monoid[LongMultiplication]

  implicit val ShortMonoid = monoid[Short]

  implicit val ShortMultiplicationMonoid = monoid[ShortMultiplication]

  implicit val FloatMonoid = monoid[Float]

  implicit val DoubleMonoid = monoid[Double]

  implicit val StringMonoid = monoid[String]

  implicit val NodeSeqMonoid = monoid[xml.NodeSeq]

  implicit def ListMonoid[A] = monoid[List[A]]

  implicit def StreamMonoid[A] = monoid[Stream[A]]

  implicit def OptionMonoid[A] = monoid[Option[A]]

  implicit def ArrayMonoid[A] = monoid[Array[A]]

  implicit def EitherLeftMonoid[A, B](implicit za: Zero[A]) = monoid[Either.LeftProjection[A, B]]

  implicit def EitherRightMonoid[A, B](implicit za: Zero[A]) = monoid[Either.RightProjection[B, A]]

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListMonoid[A] = monoid[ArrayList[A]]

  implicit def JavaLinkedListMonoid[A] = monoid[LinkedList[A]]

  implicit def JavaPriorityQueueMonoid[A] = monoid[PriorityQueue[A]]

  implicit def JavaStackMonoid[A] = monoid[Stack[A]]

  implicit def JavaVectorMonoid[A] = monoid[Vector[A]]

  implicit def JavaArrayBlockingQueueMonoid[A] = monoid[ArrayBlockingQueue[A]]

  implicit def JavaConcurrentLinkedQueueMonoid[A] = monoid[ConcurrentLinkedQueue[A]]

  implicit def JavaCopyOnWriteArrayListMonoid[A] = monoid[CopyOnWriteArrayList[A]]

  implicit def JavaLinkedBlockingQueueMonoid[A] = monoid[LinkedBlockingQueue[A]]

  implicit def JavaSynchronousQueueMonoid[A] = monoid[SynchronousQueue[A]]
}
