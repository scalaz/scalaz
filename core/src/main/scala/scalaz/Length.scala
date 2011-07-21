package scalaz

trait Length[F[_]] {
  def len[A](a: F[A]): Int

  def deriving[G[_]](implicit n: ^**^[G, F]): Length[G] =
    new Length[G] {
      def len[A](a: G[A]) =
        Length.this.len(n.unpack(a))
    }
}

object Length extends Lengths

trait Lengths {
  implicit def OptionLength: Length[Option] = new Length[Option] {
    def len[A](a: Option[A]) = a match {
      case Some(_) => 1
      case None => 0
    }
  }

  implicit def ArrayLength: Length[Array] = new Length[Array] {
    def len[A](a: Array[A]) =
      a.length
  }

  implicit def ListLength: Length[List] = new Length[List] {
    def len[A](a: List[A]) =
      a.length
  }

  implicit def ListStream: Length[Stream] = new Length[Stream] {
    def len[A](a: Stream[A]) =
      a.length
  }

  import scala.util.control.TailCalls
  import TailCalls.TailRec
  implicit def TailRecLength : Length[TailRec] = new Length[TailRec] {
    def len[A](a: TailRec[A]) =
      1
  }

  import scala.util.continuations.ControlContext
  implicit def ControlContextLength[B] : Length[({type T[A] = ControlContext[A,B,B]})#T] = new Length[({type T[A] = ControlContext[A,B,B]})#T] {
    def len[A](a: ControlContext[A,B,B]) =
      1
  }

}
