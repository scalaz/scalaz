package scalaz
package data

abstract class Lens[S, T, A, B] {
  def get(s: S): A
  def set(s: S, b: B): T

  def modify(s: S)(f: A => B): T = set(s, f(get(s)))
}
