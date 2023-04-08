package scalaz

import reflect.ClassTag
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable.{ArrayBuilder, Builder}
import collection.IndexedSeqOptimized
import syntax.Ops

/**
 * An immutable wrapper for arrays
 *
 * @tparam A type of the elements of the array
 */
sealed abstract class ImmutableArray[+A] {
  protected[this] def elemTag: ClassTag[A]

  @deprecated("removed in scalaz 7.3: not total", since = "7.2.19")
  def apply(index: Int): A
  @deprecated("removed in scalaz 7.3: not total", since = "7.2.19")
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit

  def length: Int

  def isEmpty: Boolean = length == 0

  def toArray[B >: A : ClassTag]: Array[B]

  def slice(from: Int, until: Int): ImmutableArray[A]

  def ++[B >: A: ClassTag](other: ImmutableArray[B]): ImmutableArray[B]
}

sealed abstract class ImmutableArrayInstances extends ImmutableArrayInstances1 {

  implicit def immutableArrayEqual[A](implicit A: Equal[A]): Equal[ImmutableArray[A]] =
    Equal.equal{ (a, b) =>
      (a.length == b.length) && (0 until a.length).forall(i => A.equal(a(i), b(i)))
    }

  implicit val immutableArrayInstance: Foldable[ImmutableArray] with Zip[ImmutableArray] =
    new Foldable[ImmutableArray] with Zip[ImmutableArray] {
      override def foldLeft[A, B](fa: ImmutableArray[A], z: B)(f: (B, A) => B) =
        fa.foldLeft(z)(f)
      def foldMap[A, B](fa: ImmutableArray[A])(f: A => B)(implicit F: Monoid[B]): B = {
        @annotation.tailrec
        def loop(i: Int, b: B): B = {
          if (i < fa.length) {
            loop(i + 1, F.append(b, f(fa(i))))
          } else {
            b
          }
        }
        loop(0, F.zero)
      }
      def foldRight[A, B](fa: ImmutableArray[A], z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)((a, b) => f(a, b))
      def zip[A, B](a: => ImmutableArray[A], b: => ImmutableArray[B]) = {
        val _a = a
        if(_a.isEmpty) new ImmutableArray.ofRef(Array[(A, B)]())
        else new ImmutableArray.ofRef((_a.iterator zip b.iterator).toArray)
      }
      override def index[A](fa: ImmutableArray[A], i: Int) =
        if(0 <= i && i < fa.length) Some(fa(i)) else None
      override def length[A](fa: ImmutableArray[A]) =
        fa.length
      override def empty[A](fa: ImmutableArray[A]) =
        fa.isEmpty
      override def all[A](fa: ImmutableArray[A])(f: A => Boolean) = {
        val len = fa.length
        @annotation.tailrec
        def loop(i: Int): Boolean = {
          if(i < len) f(fa(i)) && loop(i + 1)
          else true
        }
        loop(0)
      }
      override def any[A](fa: ImmutableArray[A])(f: A => Boolean) = {
        val len = fa.length
        @annotation.tailrec
        def loop(i: Int): Boolean = {
          if(i < len) f(fa(i)) || loop(i + 1)
          else false
        }
        loop(0)
      }

    }
}

object ImmutableArray extends ImmutableArrayInstances {

  def make[A](x: AnyRef): ImmutableArray[A] = {
    val y = x match {
      case null              => null
      case x: Array[Byte]    => new ofByte(x)
      case x: Array[Short]   => new ofShort(x)
      case x: Array[Char]    => new ofChar(x)
      case x: Array[Int]     => new ofInt(x)
      case x: Array[Long]    => new ofLong(x)
      case x: Array[Float]   => new ofFloat(x)
      case x: Array[Double]  => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit]    => new ofUnit(x)
      case x: Array[AnyRef]  => new ofRef(x)
      case x: String         => new StringArray(x)
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  /**
   * Wrap `x` in an `ImmutableArray`.
   *
   * Provides better type inference than `make[A]`
   */
  def fromArray[A](x: Array[A]): ImmutableArray[A] = {
    val y = x.asInstanceOf[AnyRef] match {
      case null              => null
      case x: Array[Byte]    => new ofByte(x)
      case x: Array[Short]   => new ofShort(x)
      case x: Array[Char]    => new ofChar(x)
      case x: Array[Int]     => new ofInt(x)
      case x: Array[Long]    => new ofLong(x)
      case x: Array[Float]   => new ofFloat(x)
      case x: Array[Double]  => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit]    => new ofUnit(x)
      case _: Array[AnyRef]  => new ofRef(x.asInstanceOf[Array[AnyRef]])
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  /** Wrap the characters in `str` in an `ImmutableArray` */
  def fromString(str: String): ImmutableArray[Char] = new StringArray(str)

  def newBuilder[A](implicit elemTag: ClassTag[A]): Builder[A, ImmutableArray[A]] =
    ArrayBuilder.make[A].mapResult(make(_))

  def newStringArrayBuilder: Builder[Char, ImmutableArray[Char]] =
    (new StringBuilder).mapResult(fromString(_))

  sealed abstract class ImmutableArray1[+A](array: Array[A]) extends ImmutableArray[A] {
    private[this] val arr = array.clone
    // override def stringPrefix = "ImmutableArray"
    // override protected[this] def newBuilder = ImmutableArray.newBuilder[A](elemTag)

    def componentType: Class[_] = arr.getClass().getComponentType

    def apply(idx: Int) = arr(idx)

    def length = arr.length
    def toArray[B >: A : ClassTag] = arr.clone.asInstanceOf[Array[B]]
    def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = { arr.copyToArray(xs, start, len) }

    def slice(from: Int, until: Int) = fromArray(arr.slice(from, until))

    // TODO can do O(1) for primitives
    override def ++[B >: A: ClassTag](other: ImmutableArray[B]): ImmutableArray[B] = {
      val newArr = new Array[B](length + other.length)
      this.copyToArray(newArr, 0, length)
      other.copyToArray(newArr, length, other.length)
      fromArray(newArr)
    }
  }
  final class ofRef[A <: AnyRef](array: Array[A]) extends ImmutableArray1[A](array) {
    protected[this] lazy val elemTag = ClassTag[A](componentType)
  }

  final class ofByte(array: Array[Byte]) extends ImmutableArray1[Byte](array) {
    protected[this] def elemTag = ClassTag.Byte
  }

  final class ofShort(array: Array[Short]) extends ImmutableArray1[Short](array) {
    protected[this] def elemTag = ClassTag.Short
  }

  final class ofChar(array: Array[Char]) extends ImmutableArray1[Char](array) {
    protected[this] def elemTag = ClassTag.Char

    // def mkString = new String(arr)
    // TODO why can elemTag be protected, but arr can't?
  }

  final class ofInt(array: Array[Int]) extends ImmutableArray1[Int](array) {
    protected[this] def elemTag = ClassTag.Int
  }

  final class ofLong(array: Array[Long]) extends ImmutableArray1[Long](array) {
    protected[this] def elemTag = ClassTag.Long
  }

  final class ofFloat(array: Array[Float]) extends ImmutableArray1[Float](array) {
    protected[this] def elemTag = ClassTag.Float
  }

  final class ofDouble(array: Array[Double]) extends ImmutableArray1[Double](array) {
    protected[this] def elemTag = ClassTag.Double
  }

  final class ofBoolean(array: Array[Boolean]) extends ImmutableArray1[Boolean](array) {
    protected[this] def elemTag = ClassTag.Boolean
  }

  final class ofUnit(array: Array[Unit]) extends ImmutableArray1[Unit](array) {
    protected[this] def elemTag = ClassTag.Unit
  }

  final class StringArray(val str: String) extends ImmutableArray[Char] {
    protected[this] def elemTag = ClassTag.Char

    // override protected[this] def newBuilder = (new StringBuilder).mapResult(new StringArray(_))

    def apply(idx: Int) = str(idx)

    def length = str.length
    def toArray[B >: Char : ClassTag] = str.toArray
    def copyToArray[B >: Char](xs: Array[B], start: Int, len: Int): Unit = {
      xs match {
        case xs0: Array[Char] =>
          str.copyToArray(xs0, start, len)
        case _ =>
          str.toCharArray.copyToArray(xs, start, len)
      }
    }

    def slice(from: Int, until: Int) = new StringArray(str.slice(from, until))

    def ++[B >: Char: ClassTag](other: ImmutableArray[B]) =
      other match {
        case other: StringArray => new StringArray(str + other.str)
        case _ => {
          val newArr = new Array[B](length + other.length)
          this.copyToArray(newArr, 0, length)
          other.copyToArray(newArr, length, other.length)
          fromArray(newArr)
        }
      }
  }

  implicit def wrapArray[A](immArray: ImmutableArray[A]): WrappedImmutableArray[A] = {
    import ImmutableArray.{WrappedImmutableArray => IAO}
    immArray match {
      case a: StringArray => new IAO.ofStringArray(a)
      case a: ofRef[_] => new IAO.ofRef(a)
      case a: ofByte => new IAO.ofByte(a)
      case a: ofShort => new IAO.ofShort(a)
      case a: ofChar => new IAO.ofChar(a)
      case a: ofInt => new IAO.ofInt(a)
      case a: ofLong => new IAO.ofLong(a)
      case a: ofFloat => new IAO.ofFloat(a)
      case a: ofDouble => new IAO.ofDouble(a)
      case a: ofBoolean => new IAO.ofBoolean(a)
      case a: ofUnit => new IAO.ofUnit(a)
    }
  }

  implicit def unwrapArray[A](immArrayOps: WrappedImmutableArray[A]): ImmutableArray[A] = immArrayOps.value

  abstract class WrappedImmutableArray[+A](val value: ImmutableArray[A]) extends
          IndexedSeq[A] with IndexedSeqOptimized[A, WrappedImmutableArray[A]] {
    def apply(index: Int) = value(index)
    def length = value.length

    protected[this] def arrayBuilder: Builder[A, ImmutableArray[A]]

    override protected[this] def newBuilder: Builder[A, WrappedImmutableArray[A]] = arrayBuilder.mapResult(wrapArray)
  }

  object WrappedImmutableArray {
    import scalaz.{ImmutableArray => IA}
    class ofStringArray(val strArray: StringArray) extends WrappedImmutableArray[Char](strArray) {
      override protected[this] def arrayBuilder = (new StringBuilder).mapResult(str => new StringArray(str.toString))
    }

    abstract class ofImmutableArray1[+A](val immArray: ImmutableArray1[A]) extends WrappedImmutableArray[A](immArray) {
      protected[this] def elemTag: ClassTag[A]

      override protected[this] def arrayBuilder = ImmutableArray.newBuilder[A](elemTag)
    }

    final class ofRef[+A <: AnyRef](array: IA.ofRef[A]) extends ofImmutableArray1[A](array) {
      protected[this] lazy val elemTag = ClassTag[A](array.componentType)
    }

    final class ofByte(array: IA.ofByte) extends ofImmutableArray1[Byte](array) {
      protected[this] def elemTag = ClassTag.Byte
    }

    final class ofShort(array: IA.ofShort) extends ofImmutableArray1[Short](array) {
      protected[this] def elemTag = ClassTag.Short
    }

    final class ofChar(array: IA.ofChar) extends ofImmutableArray1[Char](array) {
      protected[this] def elemTag = ClassTag.Char
    }

    final class ofInt(array: IA.ofInt) extends ofImmutableArray1[Int](array) {
      protected[this] def elemTag = ClassTag.Int
    }

    final class ofLong(array: IA.ofLong) extends ofImmutableArray1[Long](array) {
      protected[this] def elemTag = ClassTag.Long
    }

    final class ofFloat(array: IA.ofFloat) extends ofImmutableArray1[Float](array) {
      protected[this] def elemTag = ClassTag.Float
    }

    final class ofDouble(array: IA.ofDouble) extends ofImmutableArray1[Double](array) {
      protected[this] def elemTag = ClassTag.Double
    }

    final class ofBoolean(array: IA.ofBoolean) extends ofImmutableArray1[Boolean](array) {
      protected[this] def elemTag = ClassTag.Boolean
    }

    final class ofUnit(array: IA.ofUnit) extends ofImmutableArray1[Unit](array) {
      protected[this] def elemTag = ClassTag.Unit
    }
  }

  sealed class ImmutableArrayCharW(val self: ImmutableArray[Char]) extends Ops[ImmutableArray[Char]] {
    def asString: String = self match {
      case a: StringArray => a.str
      case a: ofChar => wrapArray(a).mkString
      case _ => sys.error("Unknown subtype of ImmutableArray[Char]")
    }
  }

  implicit def wrapRopeChar(array: ImmutableArray[Char]): ImmutableArrayCharW = new ImmutableArrayCharW(array)
}
