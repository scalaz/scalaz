package scalaz
package typelevel

package formatters {
  trait AllFormatters
    extends General
    with JavaLike
    with Numeric
    with String
}

package object formatters extends formatters.AllFormatters

// vim: expandtab:ts=2:sw=2
