package scalaz

package object prop {

  type ===[A, B] = Is[A, B]
  type =!=[A, B] = NotIs[A, B]

  type <~<[-A, +B] = As[A, B]
  type >~>[+B, -A] = As[A, B]

}
