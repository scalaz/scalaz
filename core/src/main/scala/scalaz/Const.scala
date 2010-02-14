package scalaz

/**
 * Used to generate Phantom Applicative Functors from a Monoidal type B.
 *
 * @see scalaz.Applicative#MonoidalApplicative
 */
case class Const[B, +A](value: B) extends NewType[B] 
