// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * A strategy for displaying a value that serves as a suitable replacement for (the defective)
 * <code>java.lang.Object.toString</code>.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Show[A] {
  /**
   * Returns the display representation of the given value.
   */
  def show(a: A): List[Char]
}

import list.NonEmptyList
import list.NonEmptyList.nel
import control.{Cofunctor, CofunctorW}
import control.CofunctorW.cofunctor
import validation.Validation

/**
 * Functions over show.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Show {
  /**
   * Construct a show with the given function.
   */
  def show[A](f: A => List[Char]) = new Show[A] {
    def show(a: A) = f(a)
  }

  /**
   * Construct a show with the given function.
   */
  def showS[A](f: A => String) = new Show[A] {
    def show(a: A) = f(a).toList
  }

  /**
   * Returns the show for the given value. 
   */
  def showv[A](a: A)(implicit s: Show[A]) = s.show(a)

  /**
   * Returns the show for the given value.
   */
  def shows[A](a: A)(implicit s: Show[A]) = s.show(a).mkString

  /**
   * Construct a show using the implementation of <code>java.lang.Object.toString</code>.
   */
  def ShowA[A]: Show[A] = new Show[A] {
    override def show(a: A) = a.toString.toList
  }

  /**
   * A show for booleans.
   */
  implicit val ShowBoolean = ShowA[Boolean]

  /**
   * A show for bytes.
   */
  implicit val ShowByte = ShowA[Byte]

  /**
   * A show for chars.
   */
  implicit val ShowChar = ShowA[Char]

  /**
   * A show for doubles.
   */
  implicit val ShowDouble = ShowA[Double]

  /**
   * A show for floats.
   */
  implicit val ShowFloat = ShowA[Float]

  /**
   * A show for integers.
   */
  implicit val ShowInt = ShowA[Int]

  /**
   * A show for longs.
   */
  implicit val ShowLong = ShowA[Long]

  /**
   * A show for shorts.
   */
  implicit val ShowShort = ShowA[Short]

  /**
   * A show for strings.
   */
  implicit val ShowString = ShowA[String]

  /**
   * A show for the unit value.
   */
  implicit val ShowUnit = ShowA[Unit]

  import control.FoldRightW._
  import control.PlusW._

  /**
   * A show for streams.
   */
  implicit def ShowStream[A](implicit sa: Show[A]): Show[Stream[A]] =
    show[Stream[A]](a => List.flatten(List('<') :: a.map(sa.show(_)).toList.intercalate[List](List(List(','))) <+> List(List('>'))))

  /**
   * A show for lists.
   */
  implicit def ShowList[A](implicit sa: Show[A]): Show[List[A]] = show[List[A]](a => List.flatten(List('[') :: a.map(sa.show(_)).intercalate[List](List(List(','))) <+> List(List(']'))))

  /**
   * A show for arrays.
   */
  implicit def ShowArray[A](implicit sa: Show[A]): Show[Array[A]] = show[Array[A]](a => {
    val s = new StringBuilder("{")

    for(i <- 0 until a.length) {
      if(i != 0)
        s += ','

      s append sa.show(a(i)).mkString
    }

    s += '}'

    s.toList
  })

  /**
   * A show for non-empty lists.
   */
  implicit def ShowNonEmptyList[A](implicit sa: Show[A]): Show[NonEmptyList[A]] = ShowList(sa) < (_.toList)

  /**
   * A show for options.
   */
  implicit def ShowOption[A](implicit sa: Show[A]): Show[Option[A]] = showS[Option[A]](o => o.map(sa.show(_).mkString).toString)

  /**
   * A show for eithers.
   */
  implicit def ShowEither[A, B](implicit sa: Show[A], sb: Show[B]): Show[Either[A, B]] = showS[Either[A, B]] {
    case Left(a) => "Left(" + sa.show(a).mkString + ')'
    case Right(b) => "Right(" + sb.show(b).mkString + ')'
  }

  /**
   * A show for validations.
   */
  implicit def ShowValidation[A, B](implicit sa: Show[A], sb: Show[B]): Show[Validation[A, B]] = showS[Validation[A, B]](_.either match {
    case Left(a) => "Fail(" + sa.show(a).mkString + ')'
    case Right(b) => "Success(" + sb.show(b).mkString + ')'
  })

  /**
   * A contra-variant functor for <code>Show</code>.
   */
  implicit val ShowCofunctor = new Cofunctor[Show] {
    def comap[A, B](f: B => A, s: Show[A]) = new Show[B] {
      def show(b: B) = s.show(f(b))
    }
  }

  /**
   * A contra-variant functor wrapper for <code>Show</code>.
   */
  implicit def ShowCofunctorW[A](s: Show[A]): CofunctorW[Show, A] = cofunctor[Show](s)


  /**
   * Prints the given value using its show implementation to the standard output stream.
   */
  def print[A](a: A)(implicit s: Show[A]) = s.show(a).foreach(Predef.print(_))

  /**
   * Prints the given value using its show implementation to the standard output stream (following by a line).
   */
  def println[A](a: A)(implicit s: Show[A]) = {
    print(a)
    Predef.println
  }
}
