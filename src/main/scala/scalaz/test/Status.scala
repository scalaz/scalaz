package scalaz.test

sealed trait Status {
  import Status._
  import S._

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
    case Exception(args, ex) => propertyException(args, ex)
  }

  def property = Property.property(this.gen)
}

object Status {
  private final case class Proven(args: List[Argument[_]]) extends Status
  private final case object Unfalsified extends Status
  private final case class Falsified(args: List[Argument[_]]) extends Status
  private final case object Undecided extends Status
  private final case class Exception(args: List[Argument[_]], ex: Throwable) extends Status

  def proven(args: List[Argument[_]]): Status = Proven(args)
  val unfalsified: Status = Unfalsified
  def falsified(args: List[Argument[_]]): Status = Falsified(args)
  val undecided: Status = Undecided
  def exception(args: List[Argument[_]], ex: Throwable): Status = Exception(args, ex)
}