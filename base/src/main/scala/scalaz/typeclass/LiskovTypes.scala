package scalaz
package typeclass

trait LiskovTypes {
  /**A convenient type alias for Liskov */
  type <~<[-A, +B] = Liskov[A, B]

  /**A flipped alias, for those used to their arrows running left to right */
  type >~>[+B, -A] = Liskov[A, B]
}
