package scalaz
import reflect.ClassManifest
import collection.{IndexedSeq, IndexedSeqOptimized}
import scala.collection.generic._
import collection.mutable.{ArrayBuffer, ArrayBuilder, Builder}

/**
 * An immutable wrapper for arrays
 *
 * @tparam A     type of the elements of the array
 */
trait ImmutableArray[+A] extends IndexedSeq[A] with IndexedSeqOptimized[A, ImmutableArray[A]] {
  protected[this] def elemManifest: ClassManifest[A]
  override def stringPrefix = "ImmutableArray"
  protected[this] override def newBuilder: Builder[A, ImmutableArray[A]] =
    error("calling newBuilder directly on ImmutableArray[A]; this should be overridden in all subclasses")
  // override def companion = ImmutableArray
}

object ImmutableArray {
  def make[A](x: AnyRef): ImmutableArray[A] = {
    val y = x match {
      case null => null
      case x: Array[Byte] => new ofByte(x)
      case x: Array[Short] => new ofShort(x)
      case x: Array[Char] => new ofChar(x)
      case x: Array[Int] => new ofInt(x)
      case x: Array[Long] => new ofLong(x)
      case x: Array[Float] => new ofFloat(x)
      case x: Array[Double] => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit] => new ofUnit(x)
      case x: Array[AnyRef] => new ofRef(x)
      case x: String => new StringArray(x)
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  /** Gives better type inference than make[A] */
  def fromArray[A](x: Array[A]): ImmutableArray[A] = {
    val y = x.asInstanceOf[AnyRef] match {
      case null => null
      case x: Array[Byte] => new ofByte(x)
      case x: Array[Short] => new ofShort(x)
      case x: Array[Char] => new ofChar(x)
      case x: Array[Int] => new ofInt(x)
      case x: Array[Long] => new ofLong(x)
      case x: Array[Float] => new ofFloat(x)
      case x: Array[Double] => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit] => new ofUnit(x)
      case y: Array[AnyRef] => new ofRef(x.asInstanceOf[Array[AnyRef]])
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  // override def newBuilder[A]: Builder[A, ImmutableArray[A]] = newBuilder(implicitly[ClassManifest[A]])
  // override def newBuilder[A]: Builder[A, ImmutableArray[A]] = (new ArrayBuffer[A]).mapResult(b => fromArray(b.toArray))

  def newBuilder[A](elemManifest: ClassManifest[A]): Builder[A, ImmutableArray[A]] =
    ArrayBuilder.make[A]()(elemManifest).mapResult(make(_))

  abstract class ImmutableArray1[+A](array: Array[A]) extends ImmutableArray[A] {
    private[this] val arr = array.clone
    override def stringPrefix = "ImmutableArray"
    override protected[this] def newBuilder = ImmutableArray.newBuilder[A](elemManifest)

    def apply(idx: Int) = arr(idx)
    def length = arr.length
    override def toArray[B >: A : ClassManifest]: Array[B] = arr.clone.asInstanceOf[Array[B]]

    override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) {
      var l = len
      if (arr.length < l) l = arr.length
      if (xs.length - start < l) l = xs.length - start max 0
      Array.copy(arr, 0, xs, start, l)
    }
  }

  final class ofRef[+T <: AnyRef](array: Array[T]) extends ImmutableArray1[T](array) {
    protected[this] lazy val elemManifest = ClassManifest.classType[T](array.getClass.getComponentType)
  }

  final class ofByte(array: Array[Byte]) extends ImmutableArray1[Byte](array) {
    protected[this] def elemManifest = ClassManifest.Byte
  }

  final class ofShort(array: Array[Short]) extends ImmutableArray1[Short](array) {
    protected[this] def elemManifest = ClassManifest.Short
  }

  final class ofChar(array: Array[Char]) extends ImmutableArray1[Char](array) {
    protected[this] def elemManifest = ClassManifest.Char
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
    override protected[this] def newBuilder = (new StringBuilder).mapResult(new StringArray(_))

    def apply(idx: Int) = str(idx)
    def length = str.length
    override def toArray[B >: Char : ClassManifest]: Array[B] = str.toArray
  }
}