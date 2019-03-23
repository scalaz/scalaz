package scalaz.example

object EnumUsage extends App {
  import scalaz._, Scalaz._, Digit._

  assert(!true.succ)
  assert(false.succ)

  assert((_2: Digit) -+- 5 === _7)
  assert((_8: Digit) --- 6 === _2)

  assert((7 |==> (2, 14)).toIList === IList(7, 9, 11, 13))
  assert((7 |--> (2, 14)) === IList(7, 9, 11, 13))
  assert((14 |==> (-2, 7)).toIList === IList(14, 12, 10, 8))
  assert((14 |--> (-2, 7)) === IList(14, 12, 10, 8))

  assert(((_4: Digit) |-> _9) === IList(_4, _5, _6, _7, _8, _9))
  assert(('m' |=> 'u').toIList === IList('m', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u'))
  assert(('m' |--> (2, 'u')) === IList('m', 'o', 'q', 's', 'u'))

}
