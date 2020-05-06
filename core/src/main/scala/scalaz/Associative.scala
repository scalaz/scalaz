package scalaz

////
/**
 *
 */
////
trait Associative[=>:[_, _]]  { self =>
  ////
  import Isomorphism.{<=>, IsoSet}

  def reassociateLeft[A, B, C](f: A =>: (B =>: C)): (A =>: B) =>: C

  def reassociateRight[A, B, C](f: (A =>: B) =>: C): A =>: (B =>: C)

  def reassociateIso[A, B, C]: ((A =>: B) =>: C) <=> (A =>: (B =>: C)) = IsoSet(reassociateRight, reassociateLeft)

  trait AssociativeLaw {
    /** Reassociating left and then right is a no-op. */
    def leftRight[A, B, C](fa: A =>: (B =>: C))(implicit FR: Equal[A =>: (B =>: C)]): Boolean =
      FR.equal(reassociateRight(reassociateLeft(fa)), fa)

    /** Reassociating right and then left is a no-op. */
    def rightLeft[A, B, C](fa: (A =>: B) =>: C)(implicit FL: Equal[(A =>: B) =>: C]): Boolean =
      FL.equal(reassociateLeft(reassociateRight(fa)), fa)
  }

  def associativeLaw = new AssociativeLaw {}
  ////
  val associativeSyntax: scalaz.syntax.AssociativeSyntax[=>:] =
    new scalaz.syntax.AssociativeSyntax[=>:] { def F = Associative.this }
}

object Associative {
  @inline def apply[F[_, _]](implicit F: Associative[F]): Associative[F] = F



  ////

  ////
}
