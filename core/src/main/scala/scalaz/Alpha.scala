package scalaz

sealed trait Alpha {
  val toChar: Char

  def toUpperChar = toChar.toUpper

  override def equals(o: Any) = o.isInstanceOf[Alpha] && o.asInstanceOf[Alpha].toChar == toChar

  override def hashCode = toChar.hashCode

  override def toString = toChar.toString
}

private case object A extends Alpha {
  val toChar = 'a'
}
private case object B extends Alpha {
  val toChar = 'b'
}
private case object C extends Alpha {
  val toChar = 'c'
}
private case object D extends Alpha {
  val toChar = 'd'
}
private case object E extends Alpha {
  val toChar = 'e'
}
private case object F extends Alpha {
  val toChar = 'f'
}
private case object G extends Alpha {
  val toChar = 'g'
}
private case object H extends Alpha {
  val toChar = 'h'
}
private case object I extends Alpha {
  val toChar = 'i'
}
private case object J extends Alpha {
  val toChar = 'j'
}
private case object K extends Alpha {
  val toChar = 'k'
}
private case object L extends Alpha {
  val toChar = 'l'
}
private case object M extends Alpha {
  val toChar = 'm'
}
private case object N extends Alpha {
  val toChar = 'n'
}
private case object O extends Alpha {
  val toChar = 'o'
}
private case object P extends Alpha {
  val toChar = 'p'
}
private case object Q extends Alpha {
  val toChar = 'q'
}
private case object R extends Alpha {
  val toChar = 'r'
}
private case object S extends Alpha {
  val toChar = 's'
}
private case object T extends Alpha {
  val toChar = 't'
}
private case object U extends Alpha {
  val toChar = 'u'
}
private case object V extends Alpha {
  val toChar = 'v'
}
private case object W extends Alpha {
  val toChar = 'w'
}
private case object X extends Alpha {
  val toChar = 'x'
}
private case object Y extends Alpha {
  val toChar = 'y'
}
private case object Z extends Alpha {
  val toChar = 'z'
}

trait Alphas {
  val alphas = List(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z)

  implicit def AlphaChar(a: Alpha) = a.toChar
}
