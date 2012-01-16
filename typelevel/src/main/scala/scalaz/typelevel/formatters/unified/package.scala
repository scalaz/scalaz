package scalaz
package typelevel.formatters

package unified {
  trait AllFormatters
    extends Numeric
    with String
}

package object unified extends unified.AllFormatters

// vim: expandtab:ts=2:sw=2

