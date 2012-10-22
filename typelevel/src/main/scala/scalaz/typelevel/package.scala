package scalaz

import Id._

/**
 * Type-level data structures in '''Scalaz'''.
 *
 * This package contains:
 *  - heterogeneous lists
 *  - type-level natural numbers
 *  - type-safe port of Java's printf
 *
 * In general, including `scalaz.typelevel._` should be enough.
 */
package object typelevel {

  /** Constraining a type constructor to a certain upper bound */
  type UpperConstrained[T[_], U] = {
    type Apply[X <: U] = T[X]
  }

  /** A [[scalaz.typelevel.GenericList]] with the type constructor [[scalaz.Id]] */
  type HList = GenericList[Id]

  /**
   * @note `::` as type and extractor is available in
   * [[scalaz.typelevel.syntax.HLists]]
   */
  type HCons[H, T <: HList] = GenericCons[Id, H, T]

  type HNil = GenericNil[Id]

  object HCons {

    def apply[H, T <: HList](head: H, tail: T): HCons[H, T] = GenericCons[Id, H, T](head, tail)

    def unapply[H, T <: HList](list: HCons[H, T]): Option[(H, T)] = Some(list.head, list.tail)
  }

  lazy val HNil: HNil = GenericNil[Id]()

  import HLists._

  implicit def mkIdOps[T <: HList](list: T): IdOps[T] = new IdOps(list)

  /** Alias for [[scalaz.typelevel.KList]] */
  type KList[M[_]] = GenericList[M]
  type KCons[M[_], H, +T <: KList[M]] = GenericCons[M, H, T]
  type KNil[M[_]] = GenericNil[M]

  object _KNil extends KNil[Nothing]

  // This is here to force that `KNil` is of type `KNil.type`,
  // and not of `object KNil`. Otherwise, there will be a type
  // mismatch if used in a pattern match.
  val KNil: _KNil.type = _KNil

  def :^: = GenericCons

  /** The empty [[scalaz.typelevel.Formatter]]. */
  def FNil[R : Monoid] =
    Formatter[HNil, R](_ => Monoid[R].zero)

}

// vim: expandtab:ts=2:sw=2
