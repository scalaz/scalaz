package scalaz

import reflect.ClassManifest
import collection.immutable.IndexedSeq
import collection.mutable.{ArrayBuilder, Builder}
import collection.generic.CanBuildFrom
import collection.IndexedSeqOptimized
import syntax.Ops

/**
 * An immutable wrapper for arrays
 *
 * @tparam A type of the elements of the array
 */
trait ImmutableArray[+A] {
  protected[this] def elemManifest: ClassManifest[A]

  def apply(index: Int): A

  def length: Int

  def isEmpty: Boolean = length == 0

  def toArray[B >: A : ClassManifest]: Array[B]
  def copyToArray[B >: A](xs: Array[B], start: Int, len: Int)
  def slice(from: Int, until: Int): ImmutableArray[A]

  def ++[B >: A](other: ImmutableArray[B]): ImmutableArray[A]
}


trait ImmutableArrayFunctions {

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

  def newBuilder[A](implicit elemManifest: ClassManifest[A]): Builder[A, ImmutableArray[A]] =
    ArrayBuilder.make[A]()(elemManifest).mapResult(make(_))

  def newStringArrayBuilder: Builder[Char, ImmutableArray[Char]] =
    (new StringBuilder).mapResult(fromString(_))

  implicit def canBuildFrom[T](implicit m: ClassManifest[T]): CanBuildFrom[ImmutableArray[_], T, ImmutableArray[T]] =
    new CanBuildFrom[ImmutableArray[_], T, ImmutableArray[T]] {
      def apply(from: ImmutableArray[_]): Builder[T, ImmutableArray[T]] = newBuilder(m)

      def apply: Builder[T, ImmutableArray[T]] = newBuilder(m)
    }

  implicit def canBuildFromChar(implicit m: ClassManifest[Char]): CanBuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] =
    new CanBuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] {
      def apply(from: ImmutableArray[_]): Builder[Char, ImmutableArray[Char]] = newStringArrayBuilder

      def apply: Builder[Char, ImmutableArray[Char]] = newStringArrayBuilder
    }

  abstract class ImmutableArray1[+A](array: Array[A]) extends ImmutableArray[A] {
    private[this] val arr = array.clone
    // override def stringPrefix = "ImmutableArray"
    // override protected[this] def newBuilder = ImmutableArray.newBuilder[A](elemManifest)

    def apply(idx: Int) = arr(idx)

    def length = arr.length
    def toArray[B >: A : ClassManifest] = arr.clone.asInstanceOf[Array[B]]
    def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) { arr.copyToArray(xs, start, len) }
    
    def slice(from: Int, until: Int) = fromArray(arr.slice(from, until))

    // TODO can do O(1) for primitives
    override def ++[B >: A](other: ImmutableArray[B]) = {
      val newArr = elemManifest.newArray(length + other.length)
      this.copyToArray(newArr, 0, length)
      other.copyToArray(newArr.asInstanceOf[Array[B]], length, other.length)
      fromArray(newArr)
    }
  }

  final class ofRef[A <: AnyRef](array: Array[A]) extends ImmutableArray1[A](array) {
    protected[this] lazy val elemManifest = ClassManifest.classType[A](array.getClass.getComponentType)
  }

  final class ofByte(array: Array[Byte]) extends ImmutableArray1[Byte](array) {
    protected[this] def elemManifest = ClassManifest.Byte
  }

  final class ofShort(array: Array[Short]) extends ImmutableArray1[Short](array) {
    protected[this] def elemManifest = ClassManifest.Short
  }

  final class ofChar(array: Array[Char]) extends ImmutableArray1[Char](array) {
    protected[this] def elemManifest = ClassManifest.Char

    // def mkString = new String(arr)
    // TODO why can elemManifest be protected, but arr can't?
  }

  final class ofInt(array: Array[Int]) extends ImmutableArray1[Int](array) {
    protected[this] def elemManifest = ClassManifest.Int
  }

  final class ofLong(array: Array[Long]) extends ImmutableArray1[Long](array) {
    protected[this] def elemManifest = ClassManifest.Long
  }

  final class ofFloat(array: Array[Float]) extends ImmutableArray1[Float](array) {
    protected[this] def elemManifest = ClassManifest.Float
  }

  final class ofDouble(array: Array[Double]) extends ImmutableArray1[Double](array) {
    protected[this] def elemManifest = ClassManifest.Double
  }

  final class ofBoolean(array: Array[Boolean]) extends ImmutableArray1[Boolean](array) {
    protected[this] def elemManifest = ClassManifest.Boolean
  }

  final class ofUnit(array: Array[Unit]) extends ImmutableArray1[Unit](array) {
    protected[this] def elemManifest = ClassManifest.Unit
  }

  final class StringArray(val str: String) extends ImmutableArray[Char] {
    protected[this] def elemManifest = ClassManifest.Char

    // override protected[this] def newBuilder = (new StringBuilder).mapResult(new StringArray(_))

    def apply(idx: Int) = str(idx)

    def length = str.length
    def toArray[B >: Char : ClassManifest] = str.toArray
    def copyToArray[B >: Char](xs: Array[B], start: Int, len: Int) { str.copyToArray(xs, start, len) }

    def slice(from: Int, until: Int) = new StringArray(str.slice(from, until))

    def ++[B >: Char](other: ImmutableArray[B]) =
      other match {
        case other: ImmutableArrayFunctions#StringArray => new StringArray(str + other.str)
        case _ => {
          val newArr = new Array[Char](length + other.length)
          this.copyToArray(newArr, 0, length)
          other.copyToArray(newArr.asInstanceOf[Array[B]], length, other.length)
          fromArray(newArr)
        }
      }
  }
}

object ImmutableArray extends ImmutableArrayFunctions {

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
  
  class WrappedImmutableArray[+A](val value: ImmutableArray[A]) extends
          IndexedSeq[A] with IndexedSeqOptimized[A, WrappedImmutableArray[A]] {
    def apply(index: Int) = value(index)
    def length = value.length

    override def stringPrefix = "ImmutableArray"

    protected[this] def arrayBuilder: Builder[A, ImmutableArray[A]] =
      sys.error("calling newBuilder directly on WrappedImmutableArray[A]; this should be overridden in all subclasses")

    override protected[this] def newBuilder: Builder[A, WrappedImmutableArray[A]] = arrayBuilder.mapResult(wrapArray)
  }

  object WrappedImmutableArray {
    import scalaz.{ImmutableArray => IA}
    class ofStringArray(val strArray: StringArray) extends WrappedImmutableArray[Char](strArray) {
      override protected[this] def arrayBuilder = (new StringBuilder).mapResult(str => new StringArray(str.toString))
    }

    abstract class ofImmutableArray1[+A](val immArray: ImmutableArray1[A]) extends WrappedImmutableArray[A](immArray) {
      protected[this] def elemManifest: ClassManifest[A]

      override protected[this] def arrayBuilder = ImmutableArray.newBuilder[A](elemManifest)
    }

    final class ofRef[+A <: AnyRef](array: IA.ofRef[A]) extends ofImmutableArray1[A](array) {
      protected[this] lazy val elemManifest = ClassManifest.classType[A](array.getClass.getComponentType)
    }

    final class ofByte(array: IA.ofByte) extends ofImmutableArray1[Byte](array) {
      protected[this] def elemManifest = ClassManifest.Byte
    }

    final class ofShort(array: IA.ofShort) extends ofImmutableArray1[Short](array) {
      protected[this] def elemManifest = ClassManifest.Short
    }

    final class ofChar(array: IA.ofChar) extends ofImmutableArray1[Char](array) {
      protected[this] def elemManifest = ClassManifest.Char
    }

    final class ofInt(array: IA.ofInt) extends ofImmutableArray1[Int](array) {
      protected[this] def elemManifest = ClassManifest.Int
    }

    final class ofLong(array: IA.ofLong) extends ofImmutableArray1[Long](array) {
      protected[this] def elemManifest = ClassManifest.Long
    }

    final class ofFloat(array: IA.ofFloat) extends ofImmutableArray1[Float](array) {
      protected[this] def elemManifest = ClassManifest.Float
    }

    final class ofDouble(array: IA.ofDouble) extends ofImmutableArray1[Double](array) {
      protected[this] def elemManifest = ClassManifest.Double
    }

    final class ofBoolean(array: IA.ofBoolean) extends ofImmutableArray1[Boolean](array) {
      protected[this] def elemManifest = ClassManifest.Boolean
    }

    final class ofUnit(array: IA.ofUnit) extends ofImmutableArray1[Unit](array) {
      protected[this] def elemManifest = ClassManifest.Unit
    }
  }

  sealed class ImmutableArrayCharW(val self: ImmutableArray[Char]) extends Ops[ImmutableArray[Char]] {
    def asString = self match {
      case a: StringArray => a.str
      case a: ofChar => wrapArray(a).mkString
      case _ => sys.error("Unknown subtype of ImmutableArray[Char]")
    }
  }

  implicit def wrapRopeChar(array: ImmutableArray[Char]): ImmutableArrayCharW = new ImmutableArrayCharW(array)
}
