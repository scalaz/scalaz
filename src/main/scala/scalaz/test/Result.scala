package scalaz.test

sealed trait Result {
  import Result._
  
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

object Result {
  private final case class Proven(args: List[Argument[_]]) extends Result
  private final case object Unfalsified extends Result
  private final case class Falsified(args: List[Argument[_]]) extends Result
  private final case object Undecided extends Result
  private final case class PropertyException(args: List[Argument[_]], ex: Throwable) extends Result
  private final case class GenException(ex: Throwable) extends Result

  def proven(args: List[Argument[_]]): Result = Proven(args)
  val unfalsified: Result = Unfalsified
  def falsified(args: List[Argument[_]]): Result = Falsified(args)
  val undecided: Result = Undecided
  def propertyException(args: List[Argument[_]], ex: Throwable): Result = PropertyException(args, ex)
  def genException(ex: Throwable): Result = GenException(ex)
}