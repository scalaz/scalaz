package scalaz.test

sealed trait Result {
  val status: Either[Throwable, Status]
  val succeeded: Int
  val discarded: Int
}

object Result {
  def result(t: Throwable, s: Int, d: Int) = new Result {
    val status = Left(t)
    val succeeded = s
    val discarded = d
  }

  def result(s: Status, su: Int, d: Int) = new Result {
    val status = Right(s)
    val succeeded = su
    val discarded = d
  }
}