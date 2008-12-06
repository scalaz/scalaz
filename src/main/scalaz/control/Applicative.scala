package scalaz.control

/**
 * Defines an applicative functor as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>.
 *
 * <p>
 * All instances must satisfy 4 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == apply(unit(identity), a)</code></li>
 * <li><strong>composition</strong><br/><code>forall af ag a. apply(af, apply(ag, a)) == apply(apply(apply(unit(compose), af), ag), a)</code></li>
 * <li><strong>homomorphism</strong><br/><code>forall f a. apply(unit(f), unit(a)) == unit(f(a))</code></li> 
 * <li><strong>interchange</strong><br/><code>forall af a. apply(af, unit(a)) == apply(unit(f => f(x)), af)</code></li>
 * </ol>
 * </p>
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Applicative[AP[_]] extends Functor[AP] with Pure[AP] with Apply[AP] {
  /**
   * The functor implementation.
   */
  final def fmap[A, B](f: A => B, fa: AP[A]) = apply(pure(f), fa)  
}

/**
 * Functions over applicatives.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Applicative {
  /**
   * Construct an applicative from the given pure and apply. These must satisfy the applicative laws.
   */
  def applicative[AP[_]](implicit p: Pure[AP], a: Apply[AP]) = new Applicative[AP] {
    def pure[A](a: A) = p.pure(a)
    def apply[A, B](f: AP[A => B], ap: AP[A]) = a(f, ap) 
  }

  /**
   * An applicative functor for identity.
   */
  implicit val IdApplicative = applicative[Tuple1]

  /**
   * An applicative functor for <code>scala.Option</code>.
   */
  implicit def OptionApplicative = applicative[Option]

  /**
   * An applicative functor for <code>scala.List</code>.
   */
  implicit def ListApplicative = applicative[List]

  /**
   * An applicative functor for <code>scalaz.list.NonEmptyList</code>.
   */
  implicit def NonEmptyListApplicative = applicative[list.NonEmptyList]

  /**
   * An applicative functor for <code>scala.Stream</code>.
   */
  implicit def StreamApplicative = applicative[Stream]

  /**
   * An applicative functor for <code>scala.Array</code>.
   */
  implicit def ArrayApplicative = applicative[Array]

  /**
   * An applicative functor for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Applicative[A] = applicative[PartialType[Function1, A]#Apply]

  /**
   * An applicative functor for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherApplicative[A] = applicative[PartialType[Either, A]#Apply]

  /**
   * An applicative functor for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherApplicative[A] = applicative[PartialType[Either, A]#Flip]

  /**
   * An applicative functor for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftApplicative[A] = applicative[PartialType[Either.LeftProjection, A]#Flip]

  /**
   * An applicative functor for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightApplicative[A] = applicative[PartialType[Either.RightProjection, A]#Apply]

  /**
   * An applicative functor for <code>forall T. scalaz.validation.Validation[T, ?]</code>.
   */
  implicit def ValidationApplicative[A](implicit s: Semigroup[A]) = applicative[PartialType[validation.Validation, A]#Apply]

  /**
   * An applicative functor for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateApplicative[A] = applicative[PartialType[State, A]#Apply]
}

/**
 * Wraps <code>Applicative</code> and a value for which there exists an instance and provides additional methods.
 *
 * @see Applicative
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait ApplicativeW[AP[_], A] extends FunctorW[AP, A] with ApplyW[AP, A] {
  /**
   * The apply implementation for this applicative.
   */
  val apply: Apply[AP]

  /**
   * The pure implementation for this applicative.
   */
  val pure: Pure[AP]

  /**
   * Maps the given applicative constant across this applicative instance.
   */
  final def *>[B](b: AP[B]) = apply(functor.fmap((a: A) => (b: B) => b, v), b)

  /**
   * Maps the given applicative constant across this applicative instance.
   */
  final def <*[B](b: AP[B]) = apply(functor.fmap((a: A) => (b: B) => a, v), b)

  /**
   * Maps the given applicative constant across this applicative instance.
   */
  final def <*|*>[B](b: AP[B]) = apply(functor.fmap((a: A) => (b: B) => (a, b), v), b)

  /**
   * Applicative lift arity-2.
   */
  final def liftA[B, C](b: AP[B], f: A => B => C) =
    apply(functor.fmap(f, v), b)

  /**
   * Applicative lift arity-3.
   */
  final def liftA[B, C, D](b: AP[B], c: AP[C], f: A => B => C => D) =
    apply(apply(functor.fmap(f, v), b), c)

  /**
   * Applicative lift arity-4.
   */
  final def liftA[B, C, D, E](b: AP[B], c: AP[C], d: AP[D], f: A => B => C => D => E) =
    apply(apply(apply(functor.fmap(f, v), b), c), d)

  /**
   * Applicative lift arity-5.
   */
  final def liftA[B, C, D, E, F](b: AP[B], c: AP[C], d: AP[D], e: AP[E], f: A => B => C => D => E => F) =
    apply(apply(apply(apply(functor.fmap(f, v), b), c), d), e)

  /**
   * Applicative lift arity-2 to product.
   */
  final def <<*>>[B](b: AP[B]) = liftA(b, a => (b: B) => (a, b))

  /**
   * Applicative lift arity-3 to product.
   */
  final def <<*>>[B, C](b: AP[B], c: AP[C]) = liftA(b, c, a => (b: B) => (c: C) => (a, b, c))

  /**
   * Applicative lift arity-4 to product.
   */
  final def <<*>>[B, C, D](b: AP[B], c: AP[C], d: AP[D]) = liftA(b, c, d, a => (b: B) => (c: C) => (d: D) => (a, b, c, d))

  /**
   * Applicative lift arity-5 to product.
   */
  final def <<*>>[B, C, D, E](b: AP[B], c: AP[C], d: AP[D], e: AP[E]) = liftA(b, c, d, e, a => (b: B) => (c: C) => (d: D) => (e: E) => (a, b, c, d, e))
}

/**
 * Defines an applicative functor as described by McBride and Paterson in
 * <a href="http://www.soi.city.ac.uk/~ross/papers/Applicative.html">Applicative Programming with Effects</a>.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object ApplicativeW {
  /**
   * Constructs an applicative from the given value and implementation.
   */
  def applicative[AP[_]]: PartialWrap[AP, Applicative, ApplicativeW] = new PartialWrap[AP, Applicative, ApplicativeW] {
    def apply[A](ap: AP[A])(implicit a: Applicative[AP]) = new ApplicativeW[AP, A] {
      val v = ap
      val apply = a
      val pure = a
      val functor: Functor[AP] = new Functor[AP] {
        def fmap[A, B](f: A => B, fa: AP[A]) = apply(a.pure(f), fa)
      }
    }
  }

  /**
   * An applicative functor for identity.
   */
  implicit def IdApplicative[A](as: Tuple1[A]) = applicative[Tuple1](as)

  /**
   * An applicative functor for <code>scala.Option</code>.
   */
  implicit def OptionApplicative[A](as: Option[A]) = applicative[Option](as)

  /**
   * An applicative functor for <code>scala.List</code>.
   */
  implicit def ListApplicative[A](as: List[A]) = applicative[List](as)

  /**
   * An applicative functor for <code>scala.Stream</code>.
   */
  implicit def StreamApplicative[A](as: Stream[A]) = applicative[Stream](as)

  /**
   * An applicative functor for <code>scala.Array</code>.
   */
  implicit def ArrayApplicative[A](as: Array[A]) = applicative[Array](as)

  /**
   * An applicative functor for <code>forall T. scala.Function1[T, ?]</code>.
   */
  implicit def Function1Applicative[A, B](as: A => B) = applicative[PartialType[Function1, A]#Apply](as)

  /**
   * An applicative functor for <code>forall T. scala.Either[T, ?]</code>.
   */
  implicit def EitherApplicative[A, B](as: Either[A, B]) = applicative[PartialType[Either, A]#Apply](as)

  /**
   * An applicative functor for <code>forall T. scala.Either[?, T]</code>.
   */
  implicit def FlipEitherApplicative[A, B](as: Either[B, A]) = applicative[PartialType[Either, A]#Flip](as)

  /**
   * An applicative functor for <code>forall T. scala.Either.LeftProjection[?, T]</code>.
   */
  implicit def EitherLeftApplicative[A, B](as: Either.LeftProjection[B, A]) = applicative[PartialType[Either.LeftProjection, A]#Flip](as)

  /**
   * An applicative functor for <code>forall T. scala.Either.RightProjection[T, ?]</code>.
   */
  implicit def EitherRightApplicative[A, B](as: Either.RightProjection[A, B]) = applicative[PartialType[Either.RightProjection, A]#Apply](as)

  import validation.Validation

  /**
   * An applicative functor for <code>forall T. scala.Validation[T, ?]</code>.
   */
  implicit def ValidationApplicative[A, B](as: Validation[A, B])(implicit s: Semigroup[A]) = applicative[PartialType[Validation, A]#Apply](as)

  /**
   * An applicative functor for <code>forall T. scalaz.State[T, ?]</code>.
   */
  implicit def StateApplicative[A, B](as: State[A, B]) = applicative[PartialType[State, A]#Apply](as)
}
