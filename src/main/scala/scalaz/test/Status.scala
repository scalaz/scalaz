package scalaz.test

sealed trait Status {
  def status[X](p: => X, t: => X, f: => X, u: => X, e: Throwable => X): X = this match {
    case Proven => p
    case Unfalsified => t
    case Falsified => f
    case Undecided => u
    case Exception(t) => e(t)
  }
  
  def isSuccess = this == Proven || this == Unfalsified

  def isFailure = this match {
    case Falsified => true
    case Exception(_) => true
    case _ => false
  }

  // identity with Proven (requires special ===)
  def &&(s: => Status): Status = this match {
    case Proven => s
    case Unfalsified => s
    case Falsified => this
    case Undecided => s match {
      case Proven => this
      case Unfalsified => this
      case Falsified => s
      case Exception(_) => s
      case Undecided => this
    }
    case Exception(_) => this
  }

  // identity with Falsified (requires special ===)
  def ||(s: => Status): Status = this match {
    case Proven => this
    case Unfalsified => this
    case Falsified => s
    case Undecided => s match {
      case Proven => s
      case Unfalsified => s
      case Falsified => this
      case Exception(_) => s
      case Undecided => this
    }
    case Exception(_) => this
  }

  def ++(s: => Status): Status = this match {
    case Proven => s
    case Unfalsified => s
    case Falsified => this
    case Undecided => s
    case Exception(_) => this
  }
}
private case object Proven extends Status
private case object Unfalsified extends Status
private case object Falsified extends Status
private case object Undecided extends Status
private final case class Exception(e: Throwable) extends Status

object Status {
  val proven: Status = Proven
  val unfalsified: Status = Unfalsified
  val falsified: Status = Falsified
  val undecided: Status = Undecided
  def exception(e: Throwable): Status = Exception(e)
}