package scalaz

/**
 * Implicits to provide a convenient syntax to work with type classes
 * and functions.
 *
 * Non-trivial code should *not* be defined in this package; instead delegate.
 */
package object syntax extends Syntaxes {
  object strictTree extends ToStrictTreeOps
}
