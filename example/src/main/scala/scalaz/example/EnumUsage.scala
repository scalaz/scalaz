package scalaz.example

object EnumUsage extends App {
  import scalaz._, Scalaz._, Digit._

  assert(!true.succ)
  assert(false.succ)

  assert((_2: Digit) -+- 5 === _7)
  assert((_8: Digit) --- 6 === _2)

  assert((7 |==> (2, 14)).toList === List(7, 9, 11, 13))
  assert((14 |==> (-2, 7)).toList === List(14, 12, 10, 8))

}
