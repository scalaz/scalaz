package scalaz

trait Index[F[_]] {
  def index[A](a: F[A]): Int => Option[A]

  def indexOr[A](a: F[A], d: => A): Int => A =
    n => index(a)(n) getOrElse d

  def deriving[G[_]](implicit n: ^**^[G, F]): Index[G] =
    new Index[G] {
      def index[A](a: G[A]) =
        Index.this.index(n.unpack(a))
    }
}

object Index extends Indexs

trait Indexs {
  implicit def OptionIndex: Index[Option] = new Index[Option] {
    def index[A](a: Option[A]) = i => a filter (_ => i == 0)
  }

  implicit def ArrayIndex: Index[Array] = new Index[Array] {
    def index[A](a: Array[A]) =
      i => if (i >= 0 && i < a.length) Some(a(i)) else None
  }

  implicit def ListIndex: Index[List] = new Index[List] {
    def index[A](a: List[A]) =
      i => {
        var n = 0
        var k: Option[A] = None
        val it = a.iterator
        while (it.hasNext && k.isEmpty) {
          val z = it.next
          if (n == i) k = Some(z)
          n = n + 1
        }

        k
      }
  }

  implicit def StreamIndex: Index[Stream] = new Index[Stream] {
    def index[A](a: Stream[A]) =
      i => {
        var n = 0
        var k: Option[A] = None
        val it = a.iterator
        while (it.hasNext && k.isEmpty) {
          val z = it.next
          if (n == i) k = Some(z)
          n = n + 1
        }

        k
      }
  }

  import scala.util.control.TailCalls
  import TailCalls.TailRec
  implicit def TailRecLength : Index[TailRec] = new Index[TailRec] {
    def index[A](a: TailRec[A]) =
      n => if(n == 0) Some(a.result) else None
  }

  import scala.util.continuations.ControlContext
  implicit def ControlContextLength[B] : Index[({type T[A] = ControlContext[A,B,B]})#T] = new Index[({type T[A] = ControlContext[A,B,B]})#T] {
    def index[A](a: ControlContext[A,B,B]) =
      n => if(n == 0) Some(a.x) else None
  }

}
