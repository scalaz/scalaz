package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Equal` */
trait EqualOps[F] extends Ops[F] {
  implicit def F: Equal[F]
  ////

  final def ===(other: F): Boolean = F.equal(self, other)
  final def /==(other: F): Boolean = !F.equal(self, other)
  final def =/=(other: F): Boolean = /==(other)
  final def ≟(other: F): Boolean = F.equal(self, other)
  final def ≠(other: F): Boolean = !F.equal(self, other)

  /** Raises an exception unless self === other. */
  final def assert_===[B](other: B)(implicit S: Show[F], ev: B <:< F) =
      if (/==(other)) sys.error(S.shows(self) + " ≠ " + S.shows(ev(other)))

  ////
}

trait ToEqualOps  {
  implicit def ToEqualOps[F](v: F)(implicit F0: Equal[F]) =
    new EqualOps[F] { def self = v; implicit def F: Equal[F] = F0 }

  ////

  ////
}

trait EqualSyntax[F]  {
  implicit def ToEqualOps(v: F)(implicit F0: Equal[F]): EqualOps[F] = new EqualOps[F] { def self = v; implicit def F: Equal[F] = F0 }

  ////

  ////
}
