package scalaz

/*
Laws

1) succ(pred(x)) === x
2) pred(succ(x)) === x
3) min forall (n => max forall (x => pred(n) === x))
4) min forall (n => max forall (x => succ(x) === n))
5) succn(1) === succ
6) predn(1) === pred
*/
sealed trait Enum[A] extends Order[A] {
  val succ: A => A
  val pred: A => A
  val succn: Int => A => A
  val predn: Int => A => A
  val min: Option[A]
  val max: Option[A]
}

object Enum {
  def enum[A](
    succ: A => A
  , pred: A => A
  , succn: Int => A => A
  , predn: Int => A => A
  , min: Option[A] = None
  , max: Option[A] = None
  )(implicit o: Order[A]): Enum[A] = {
    val s = succ
    val p = pred
    val sn = succn
    val pn = predn
    val mn = min
    val mx = max
    new Enum[A] {
      val succ = s
      val pred = p
      val succn = sn
      val predn = pn
      val min = mn
      val max = mx
      def order(x: A, y: A) = o.order(x, y)
    }
  }

  def nooptEnum[A](
  succ: A => A
  , pred: A => A
  , min: Option[A] = None
  , max: Option[A] = None
  )(implicit o: Order[A]): Enum[A] =
    enum(
      succ
      , pred
      , n => a => {
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
      , n => a => {
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
      , min
      , max
    )


  def boundedEnum[A](
    succ: A => A
  , pred: A => A
  , succn: Int => A => A
  , predn: Int => A => A
  , min: A
  , max: A
  )(implicit o: Order[A]): Enum[A] =
    enum(
      succ
    , pred
    , succn
    , predn
    , Some(min)
    , Some(max)
    )

  def boundedNoptEnum[A](
    succ: A => A
    , pred: A => A
    , min: A
    , max: A
  )(implicit o: Order[A]): Enum[A] =
    nooptEnum(
      succ
      , pred
      , Some(min)
      , Some(max)
    )

  implicit val UnitEnum: Enum[Unit] =
    boundedEnum(
      succ = _ => ()
      , pred = _ => ()
      , succn = _ => _ => ()
      , predn = _ => _ => ()
      , min = ()
      , max = ()
    )

  implicit val BooleanEnum: Enum[Boolean] =
    boundedEnum(
      succ = !_
      , pred = !_
      , succn = a => b => if(a % 2 == 0) b else !b
      , predn = a => b => if(a % 2 == 0) b else !b
      , min = false
      , max = true
    )

  implicit val OrderingEnum: Enum[Ordering] =
    boundedEnum(
      succ = {
        case Ordering.LT => Ordering.EQ
        case Ordering.EQ => Ordering.GT
        case Ordering.GT => Ordering.LT
      }
      , pred = {
        case Ordering.GT => Ordering.EQ
        case Ordering.EQ => Ordering.LT
        case Ordering.LT => Ordering.GT
      }
      , succn = a => b => error("")
      , predn = a => b => error("")
      , min = Ordering.LT
      , max = Ordering.GT
    )

  implicit val IntEnum: Enum[Int] =
    boundedEnum(
      succ = 1+
      , pred = 1-
      , succn = a => b => b + a
      , predn = a => b => b - a
      , min = Int.MinValue
      , max = Int.MaxValue
    )
}
