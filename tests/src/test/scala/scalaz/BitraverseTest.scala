package scalaz

import scalaz.std.AllInstances.{tuple2Instance => _, _}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class BitraverseTest extends Spec {

  implicit val LE = Bitraverse[\/].leftTraverse[Int]
  implicit val RE = Bitraverse[\/].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for Either",  traverse.laws[({type λ[α] = α \/ Int})#λ])
  checkAll("Right-biased Bitraverse for Either", traverse.laws[({type λ[α] = Int \/ α})#λ])


  implicit val LT = Bitraverse[Tuple2].leftTraverse[Int]
  implicit val RT = Bitraverse[Tuple2].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for (,)",  traverse.laws[({type λ[α] = (α, Int)})#λ])
  checkAll("Right-biased Bitraverse for (,)", traverse.laws[({type λ[α] = (Int, α)})#λ])


  "left/right bias" in {
    import scalaz.syntax.either._

    Bitraverse[\/].rightTraverse.traverse(42.left[Int])(x => Vector(x + 3)) must be_===(Vector(-\/(42)))
    Bitraverse[\/].leftTraverse.traverse(42.left[Int])(x => Vector(x + 3))  must be_===(Vector(-\/(45)))

    Bifoldable[\/].leftFoldable.foldMap(42.left[Int])(identity)  must be_===(42)
    Bifoldable[\/].rightFoldable.foldMap(42.left[Int])(identity) must be_===(0)
  }

}

// vim: expandtab:ts=2:sw=2
