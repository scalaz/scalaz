package scalaz
package typeclass

trait ProfunctorClass[F[_, _]] extends Profunctor[F] {
  final def profunctor: Profunctor[F] = this
}

object ProfunctorClass {

  trait Template[F[_, _]] extends ProfunctorClass[F] with Profunctor.LeftRightMap[F]

  trait AltTemplate[F[_, _]] extends ProfunctorClass[F] with Profunctor.Dimap[F]

}