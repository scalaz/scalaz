package scalaz

/** An algebraic data type representing the characters 'a' to 'z' */
sealed abstract class Alpha extends Product with Serializable {
  val toChar: Char

  def toUpperChar: Char = toChar.toUpper
}

object Alpha extends AlphaInstances {

  case object A extends Alpha {
    val toChar = 'a'
  }

  case object B extends Alpha {
    val toChar = 'b'
  }

  case object C extends Alpha {
    val toChar = 'c'
  }

  case object D extends Alpha {
    val toChar = 'd'
  }

  case object E extends Alpha {
    val toChar = 'e'
  }

  case object F extends Alpha {
    val toChar = 'f'
  }

  case object G extends Alpha {
    val toChar = 'g'
  }

  case object H extends Alpha {
    val toChar = 'h'
  }

  case object I extends Alpha {
    val toChar = 'i'
  }

  case object J extends Alpha {
    val toChar = 'j'
  }

  case object K extends Alpha {
    val toChar = 'k'
  }

  case object L extends Alpha {
    val toChar = 'l'
  }

  case object M extends Alpha {
    val toChar = 'm'
  }

  case object N extends Alpha {
    val toChar = 'n'
  }

  case object O extends Alpha {
    val toChar = 'o'
  }

  case object P extends Alpha {
    val toChar = 'p'
  }

  case object Q extends Alpha {
    val toChar = 'q'
  }

  case object R extends Alpha {
    val toChar = 'r'
  }

  case object S extends Alpha {
    val toChar = 's'
  }

  case object T extends Alpha {
    val toChar = 't'
  }

  case object U extends Alpha {
    val toChar = 'u'
  }

  case object V extends Alpha {
    val toChar = 'v'
  }

  case object W extends Alpha {
    val toChar = 'w'
  }

  case object X extends Alpha {
    val toChar = 'x'
  }

  case object Y extends Alpha {
    val toChar = 'y'
  }

  case object Z extends Alpha {
    val toChar = 'z'
  }

  val alphas: List[Alpha] = List(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

  implicit def ToCharFromAlpha(a: Alpha): Char = a.toChar

}

sealed abstract class AlphaInstances {
  // TODO
}
