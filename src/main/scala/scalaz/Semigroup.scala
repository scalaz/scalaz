package scalaz

trait Semigroup[S] {
  def append(s1: S, s2: => S): S
}

object Semigroup {
  def semigroup[S](f: (S, S) => S) = new Semigroup[S] {
    def append(s1: S, s2: => S) = f(s1, s2)
  }

  implicit val DigitSemigroup: Semigroup[Digit] = semigroup[Digit]((a, b) => a.toInt + b.toInt)

  implicit val OrderingSemigroup = semigroup[Ordering] {
    case (EQ, a) => a
    case (LT, _) => LT
    case (GT, _) => GT
  }

  import S._
  
  implicit val UnitSemigroup = semigroup[Unit]((_, _) => ())

  implicit val IntSemigroup: Semigroup[Int] = semigroup[Int](_ + _)

  implicit val BooleanConjunctionSemigroup: Semigroup[BooleanConjunction] = semigroup[BooleanConjunction](_ && _ |&&|)

  implicit val BooleanSemigroup = semigroup[Boolean](_ || _)

  implicit val CharSemigroup: Semigroup[Char] = semigroup[Char]((a, b) => (a + b).toChar)

  implicit val ByteSemigroup: Semigroup[Byte] = semigroup[Byte]((a, b) => (a + b).toByte)

  implicit val LongSemigroup: Semigroup[Long] = semigroup[Long]((a, b) => (a + b).toLong)

  implicit val ShortSemigroup: Semigroup[Short] = semigroup[Short]((a, b) => (a + b).toShort)

  implicit val FloatSemigroup: Semigroup[Float] = semigroup[Float]((a, b) => (a + b).toFloat)

  implicit val DoubleSemigroup: Semigroup[Double] = semigroup[Double]((a, b) => (a + b).toDouble)

  implicit val StringSemigroup = semigroup[String](_ + _)

  implicit def NonEmptyListSemigroup[A] = semigroup[NonEmptyList[A]](_.list <::: _)

  implicit def ListSemigroup[A] = semigroup[List[A]](_ ::: _)

  implicit def StreamSemigroup[A] = semigroup[Stream[A]](_ append _)

  implicit def OptionSemigroup[A] = semigroup[Option[A]]((a, b) => if(a.isDefined) a else b)

  implicit def ArraySemigroup[A] = semigroup[Array[A]](_ ++ _)

  implicit def EitherLeftSemigroup[A, B] = semigroup[Either.LeftProjection[A, B]]((a, b) => if(a.e.isRight) a else b)

  implicit def EitherRightSemigroup[A, B] = semigroup[Either.RightProjection[B, A]]((a, b) => if(a.e.isLeft) a else b)

  import java.util._
  import java.util.concurrent._

  implicit def JavaArrayListSemigroup[A] = semigroup[ArrayList[A]]((a, b) => {
    val k = a.clone.asInstanceOf[ArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedListSemigroup[A] = semigroup[LinkedList[A]]((a, b) => {
    val k = a.clone.asInstanceOf[LinkedList[A]]
    k addAll b
    k
  })

  implicit def JavaPriorityQueueSemigroup[A] = semigroup[PriorityQueue[A]]((a, b) => {
    val k = new PriorityQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaStackSemigroup[A] = semigroup[Stack[A]]((a, b) => {
    val k = a.clone.asInstanceOf[Stack[A]]
    k addAll b
    k
  })

  implicit def JavaVectorSemigroup[A] = semigroup[Vector[A]]((a, b) => {
    val k = a.clone.asInstanceOf[Vector[A]]
    k addAll b
    k
  })

  implicit def JavaArrayBlockingQueueSemigroup[A] = semigroup[ArrayBlockingQueue[A]]((a, b) => {
    val k = new ArrayBlockingQueue[A](a.remainingCapacity + b.remainingCapacity)
    k addAll a
    k addAll b
    k
  })

  implicit def JavaConcurrentLinkedQueueSemigroup[A] = semigroup[ConcurrentLinkedQueue[A]]((a, b) => {
    val k = new ConcurrentLinkedQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaCopyOnWriteArrayListSemigroup[A] = semigroup[CopyOnWriteArrayList[A]]((a, b) => {
    val k = a.clone.asInstanceOf[CopyOnWriteArrayList[A]]
    k addAll b
    k
  })

  implicit def JavaLinkedBlockingQueueSemigroup[A] = semigroup[LinkedBlockingQueue[A]]((a, b) => {
    val k = new LinkedBlockingQueue[A](a)
    k addAll b
    k
  })

  implicit def JavaSynchronousQueueSemigroup[A] = semigroup[SynchronousQueue[A]]((a, b) => {
    val k = new SynchronousQueue[A]
    k addAll a
    k addAll b
    k
  })
}
