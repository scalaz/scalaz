package scalaz.control

/**
 * A paramorphism of an iterable that can be deconstructed into next element and remaining elements.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Paramorphism[P[_]] {
  /**
   * Fold the given function across the given environment.
   *
   * @param fa The environment to fold across.
   * @param b The beginning value of the fold.
   * @param f The function to fold.
   */
  def para[A, B](fa: P[A], b: B, f: (=> A, => P[A], B) => B): B
}

import list.NonEmptyList
import list.NonEmptyList.nel

/**
 * Functions over paramorphisms.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Paramorphism {
  /**
   * A paramorphism for <code>scala.Option</code>.
   */
  implicit val OptionParamorphism = new Paramorphism[Option] {
    override def para[A, B](as: Option[A], b: B, f: ((=> A, => Option[A], B) => B)): B = as match {
      case None => b
      case Some(a) => f(a, None, b)
    }
  }

  /**
   * A paramorphism for <code>scala.List</code>.
   */
  implicit val ListParamorphism = new Paramorphism[List] {
    override def para[A, B](as: List[A], b: B, f: ((=> A, => List[A], B) => B)): B = as match {
      case Nil => b
      case a :: as => f(a, as, para(as, b, f))
    }
  }

  /**
   * A paramorphism for <code>NonEmptyList</code>.
   */
  implicit val NonEmptyListParamorphism = new Paramorphism[NonEmptyList] {
    override def para[A, B](as: NonEmptyList[A], b: B, f: ((=> A, => NonEmptyList[A], B) => B)): B = as match {
      case NonEmptyList(a, Nil) => f(a, as, b)
      case NonEmptyList(aa, a :: as) => {
        val z = nel(a, as)
        f(aa, z, para(z, b, f))
      }
    }
  }

  /**
   * A paramorphism for <code>scala.Stream</code>.
   */
  implicit val StreamParamorphism = new Paramorphism[Stream] {
    override def para[A, B](as: Stream[A], b: B, f: ((=> A, => Stream[A], B) => B)): B =
      if(as.isEmpty)
        b
      else
        f(as.head, as.tail, para(as.tail, b, f))
  }
}

/**
 * Wraps <code>Paramorphism</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Paramorphism
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait ParamorphismW[P[_], A] {
  /**
   * The paramorphism value.
   */
  val v: P[A]

  /**
   * The implementation for the paramorphism value.
   */
  val paramorphism: Paramorphism[P]

  /**
   * Performs a paramorphic fold across this iterable.
   */
  def para[B](b: B, f: (=> A, => P[A], B) => B) = paramorphism.para(v, b, f)

  import SemigroupW._

  /**
   * Drops from this environment while the given predicate satisfies. i.e. a generalised <code>dropWhile</code>.
   */
  def dropSelect(f: A => Boolean)(implicit e: Empty[P], p: Pure[P], m: Monoid[P[A]]) =
    para[P[A]](e.empty, (a, as, b) => if(f(a)) b else p.pure(a) |+| as)

  /**
   * Splits this iterable using the given predicate. The left of the tuple contains the first elements that satisfy
   * the given predicate.
   * e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] |- (_ % 2 == 0)</code> yields
   * </p>
   * <p>
   * <code>([2, 6, 8],[9, 6, 7, 3, 5, 8, 6, 9])</code>
   * </p>
   */
  def |-(p: A => Boolean)(implicit pr: Pure[P], e: Empty[P], mo: Monoid[P[A]]) =
    para[(P[A], P[A])]((e.empty, e.empty), (a, as, asbs) =>
            if(p(a)) (pr.pure(a) |+| asbs._1, asbs._2)
            else (e.empty, pr.pure(a) |+| as))
  
  /**
   * Splits this iterable using the given predicate. The left of the tuple contains the first elements that do not
   * satisfy the given predicate.
   * e.g.
   * <p>
   * <code>[2, 6, 8, 9, 6, 7, 3, 5, 8, 6, 9] !- (_ % 2 != 0)</code> yields
   * </p>
   * <p>
   * <code>([2, 6, 8],[9, 6, 7, 3, 5, 8, 6, 9])</code>
   * </p>
   */
  def !-(p: A => Boolean)(implicit pr: Pure[P], e: Empty[P], mo: Monoid[P[A]]) = |-(!p(_))  
}

/**
 * Functions over paramorphism values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ParamorphismW {
  /**
   * Used to partially apply a higher-kinded argument when wrapping paramorphism.
   *
   * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
   * @version $LastChangedRevision$<br>
   *          $LastChangedDate$<br>
   *          $LastChangedBy$
   */
  trait PartialWrap[T[_], U[_[_]], V[_[_], _]] {
    /**
     * Completes the application with inference.
     */
    def apply[A](a: => T[A])(implicit t: U[T]): V[T, A]
  }

  /**
   * Constructs a paramorphism from the given value and implementation.
   */
  def paramorphism[P[_]] = new PartialWrap[P, Paramorphism, ParamorphismW] {
    def apply[A](pa: => P[A])(implicit p: Paramorphism[P]) = new ParamorphismW[P, A] {
      val v = pa
      val paramorphism = p
    }
  }

  /**
   * A paramorphism for <code>scala.Option</code>.
   */
  implicit def OptionParamorphism[A](as: Option[A]) = paramorphism[Option](as)

  /**
   * A paramorphism for <code>scala.List</code>.
   */
  implicit def ListParamorphism[A](as: List[A]) = paramorphism[List](as)

  import list.NonEmptyList

  /**
   * A paramorphism for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListParamorphism[A](as: NonEmptyList[A]) = paramorphism[NonEmptyList](as)

  /**
   * A paramorphism for <code>scala.Stream</code>.
   */
  implicit def StreamParamorphism[A](as: Stream[A]): ParamorphismW[Stream, A] = paramorphism[Stream](as)
}
