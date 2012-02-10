package scalaz

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
