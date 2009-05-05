package scalaz.test

// todo this replaces Result, which is bogus (and everything depending on it i.e. Testable, Property)
sealed trait CheckStatus {
  import CheckStatus._
  
  def fold[X](proven: List[Argument[_]] => X,
              unfalsified: => X,
              falsified: List[Argument[_]] => X,
              undecided: => X,
              propertyException: (List[Argument[_]], Throwable) => X,
              genException: Throwable => X): X = this match {
    case Proven(args) => proven(args)
    case Unfalsified => unfalsified
    case Falsified(args) => falsified(args)
    case Undecided => undecided
    case PropertyException(args, ex) => propertyException(args, ex)
    case GenException(ex) => genException(ex)
  }
}

object CheckStatus {
  private final case class Proven(args: List[Argument[_]]) extends CheckStatus
  private final case object Unfalsified extends CheckStatus
  private final case class Falsified(args: List[Argument[_]]) extends CheckStatus
  private final case object Undecided extends CheckStatus
  private final case class PropertyException(args: List[Argument[_]], ex: Throwable) extends CheckStatus
  private final case class GenException(ex: Throwable) extends CheckStatus
}