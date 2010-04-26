package scalaz

/**
 * Binary covariant functor.
 *
 * <p>
 * All binary functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == bimap(a, identity, identity)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g h i. bimap(a, f compose g, h compose i) == bimap(bimap(a, g, i), f, h)</code></li>
 * </ol>
 * </p>
 */
trait Bifunctor[F[_, _]] {
  def bimap[A, B, C, D](k: F[A, B], f: A => C, g: B => D): F[C, D]
}

object Bifunctor {
  import Scalaz._
  
  implicit def Tuple2Bifunctor: Bifunctor[Tuple2] = new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](k: (A, B), f: A => C, g: B => D) =
      (f(k._1), g(k._2))
  }

  implicit def EitherBifunctor: Bifunctor[Either] = new Bifunctor[Either] {
    def bimap[A, B, C, D](k: Either[A, B], f: A => C, g: B => D) =
      k match {
        case Left(a) => Left(f(a))
        case Right(b) => Right(g(b))
      }
  }

  implicit def ValidationBifunctor: Bifunctor[Validation] = new Bifunctor[Validation] {
    def bimap[A, B, C, D](k: Validation[A, B], f: A => C, g: B => D) =
      k match {
        case Failure(a) => failure(f(a))
        case Success(b) => success(g(b))
      }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryBifunctor: Bifunctor[Entry] = new Bifunctor[Entry] {
    def bimap[A, B, C, D](k: Entry[A, B], f: A => C, g: B => D) = new SimpleImmutableEntry(f(k.getKey), g(k.getValue))
  }

}
