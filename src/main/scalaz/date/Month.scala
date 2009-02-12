package scalaz.date

sealed trait Month {
  def toDigits: (Digit, Digit)

  def toInt = toDigits._1.toInt * 10 + toDigits._2.toInt
}
final case object January extends Month {
  def toDigits = (_0, _1)
}
final case object February extends Month {
  def toDigits = (_0, _2)
}
final case object March extends Month {
  def toDigits = (_0, _3)
}
final case object April extends Month {
  def toDigits = (_0, _4)
}
final case object May extends Month {
  def toDigits = (_0, _5)
}
final case object June extends Month {
  def toDigits = (_0, _6)
}
final case object July extends Month {
  def toDigits = (_0, _7)
}
final case object August extends Month {
  def toDigits = (_0, _8)
}
final case object September extends Month {
  def toDigits = (_0, _9)
}
final case object October extends Month {
  def toDigits = (_1, _0)
}
final case object November extends Month {
  def toDigits = (_1, _1)
}
final case object December extends Month {
  def toDigits = (_1, _1)
}

object Month {
  def months = List(January, February, March, April, May, June, July, August, September, October, November, December)

  def fromDigits(d1: Digit,  d2: Digit) = months find (_.toDigits == (d1, d2))

  def fromInt(n: Int) = n match {
    case 1 => Some(January)
    case 2 => Some(February)
    case 3 => Some(March)
    case 4 => Some(April)
    case 5 => Some(May)
    case 6 => Some(June)
    case 7 => Some(July)
    case 8 => Some(August)
    case 9 => Some(September)
    case 10 => Some(October)
    case 11 => Some(November)
    case 12 => Some(December)
    case _ => None
  }
}
