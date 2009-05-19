package scalaz

sealed trait Alpha {
  val toChar: Char

  def toUpperChar = toChar.toUpperCase

  override def equals(o: Any) = o.isInstanceOf[Alpha] && o.asInstanceOf[Alpha].toChar == toChar

  override def hashCode = toChar.hashCode

  override def toString = toChar.toString
}
private final case object A extends Alpha {
  val toChar = 'a'
}
private final case object B extends Alpha {
  val toChar = 'b'
}
private final case object C extends Alpha {
  val toChar = 'c'
}
private final case object D extends Alpha {
  val toChar = 'd'
}
private final case object E extends Alpha {
  val toChar = 'e'
}
private final case object F extends Alpha {
  val toChar = 'f'
}
private final case object G extends Alpha {
  val toChar = 'g'
}
private final case object H extends Alpha {
  val toChar = 'h'
}
private final case object I extends Alpha {
  val toChar = 'i'
}
private final case object J extends Alpha {
  val toChar = 'j'
}
private final case object K extends Alpha {
  val toChar = 'k'
}
private final case object L extends Alpha {
  val toChar = 'l'
}
private final case object M extends Alpha {
  val toChar = 'm'
}
private final case object N extends Alpha {
  val toChar = 'n'
}
private final case object O extends Alpha {
  val toChar = 'o'
}
private final case object P extends Alpha {
  val toChar = 'p'
}
private final case object Q extends Alpha {
  val toChar = 'q'
}
private final case object R extends Alpha {
  val toChar = 'r'
}
private final case object S extends Alpha {
  val toChar = 's'
}
private final case object T extends Alpha {
  val toChar = 't'
}
private final case object U extends Alpha {
  val toChar = 'u'
}
private final case object V extends Alpha {
  val toChar = 'v'
}
private final case object W extends Alpha {
  val toChar = 'w'
}
private final case object X extends Alpha {
  val toChar = 'x'
}
private final case object Y extends Alpha {
  val toChar = 'y'
}
private final case object Z extends Alpha {
  val toChar = 'z'  
}

object Alpha {
  val alphas = List(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

  implicit def AlphaChar(a: Alpha) = a.toChar
}
