package scalaz

/*
Laws

1) succ(pred(x)) === x
2) pred(succ(x)) === x
3) min forall (n => max forall (x => pred(n) === x))
4) min forall (n => max forall (x => succ(x) === n))
5) succn(1)(x) === succ(x)
6) predn(1)(x) === pred(x)
7) compare(succ(x), x) != LT
8) compare(pred(x), x) != GT
*/
trait Enum[A] extends Order[A] {
  def succ: A => A
  def pred: A => A
  def succn: Int => A => A =
    n => a => {
      var w = n
      var z = a
      while(w < 0) {
        z = pred(a)
        w = w + 1
      }
      while(w > 0) {
        z = succ(a)
        w = w - 1
      }
      z
    }
  def predn: Int => A => A =
    n => a => {
      var w = n
      var z = a
      while(w < 0) {
        z = succ(a)
        w = w + 1
      }
      while(w > 0) {
        z = pred(a)
        w = w - 1
      }
      z
    }
  def min: Option[A] =
    None
  def max: Option[A] =
    None
}
