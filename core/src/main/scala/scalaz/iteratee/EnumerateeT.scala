package scalaz.iteratee


sealed trait EnumerateeT[I, O, F[_], A] {
  val apply: IterateeT[I, F, A] => IterateeT[O, F, IterateeT[I, F, A]]
}

object EnumerateeT extends EnumerateeTs

trait EnumerateeTs {
  def enumerateeT[I, O, F[_], A](k: IterateeT[I, F, A] => IterateeT[O, F, IterateeT[I, F, A]]): EnumerateeT[I, O, F, A] =
    new EnumerateeT[I, O, F, A] {
      val apply = k
    }
}