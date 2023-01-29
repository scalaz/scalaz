package scalaz

////
////
trait ArrowParent[=>:[_, _]] { self: Arrow[=>:] =>
  ////

  /** Swaps a pair. */
  def swap[X, Y]: ((X, Y) =>: (Y, X)) = arr[(X, Y), (Y, X)] {
    case (x, y) => (y, x)
  }

  ////
}
