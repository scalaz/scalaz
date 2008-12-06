package scalaz.control

/**
 * A monad with plus.
 *
 * <p>
 * All monad-plus instances must satisfy the monad laws and 1 additional law:
 * <ol>
 * <li><strong>associativity</strong><br/><code>forall a b c. plus(a, plus(a, b)) == plus(plus(a, b), c)</code></li>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MonadPlus[M[_]] extends Monad[M] with Plus[M]

object MonadPlus {
  /**
   * Construct a monad-plus from the given monad and plus. These must satisfy the monad-plus laws.
   */
  def monadPlus[M[_]](implicit m: Monad[M], p: Plus[M]) = new MonadPlus[M] {
    def pure[A](a: A) = m.pure(a)
    def bind[A, B](f: A => M[B], a: M[A]) = m.bind(f, a)
    def plus[A](m1: => M[A], m2: => M[A]) = p.plus(m1, m2)
  }

  /**
   * A monad-plus for <code>scala.Option</code>.
   */
  implicit val OptionMonadPlus = monadPlus[Option]

  /**
   * A monad-plus for <code>scala.Option</code>.
   */
  implicit val ListMonadPlus = monadPlus[List]

  import list.NonEmptyList

  /**
   * A monad-plus for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit val NonEmptyListMonadPlus = monadPlus[NonEmptyList]

  /**
   * A monad-plus for <code>scala.Stream</code>.
   */
  implicit val StreamMonadPlus = monadPlus[Stream]

  /**
   * A monad-plus for <code>scala.Array</code>.
   */
  implicit val ArrayMonadPlus = monadPlus[Array]

  /**
   * A monad-plus for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherMonadPlus[X] = monadPlus[PartialType[Either, X]#Apply]

  /**
   * A monad-plus for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherMonadPlus[X] = monadPlus[PartialType[Either, X]#Flip]

  /**
   * A monad-plus for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftMonadPlus[X] = monadPlus[PartialType[Either.LeftProjection, X]#Flip]

  /**
   * A monad-plus for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightMonadPlus[X] = monadPlus[PartialType[Either.RightProjection, X]#Apply]

  /**
   * If the given condition is <code>true</code> then return the monadic unit value, otherwise, return the given empty,
   * which may contain a side-effect.
   */
  def guard[M[_]](c: Boolean)(implicit m: MonadEmpty[M]): M[Unit] =
    if(c) m.pure(()) else m.empty

  /**
   * If the given condition is <code>false</code> then return the monadic unit value, otherwise, return the given empty,
   * which may contain a side-effect.
   */
  def prevent[M[_]](c: Boolean)(implicit m: MonadEmpty[M]): M[Unit] =
    if(c) m.empty else m.pure(())
}