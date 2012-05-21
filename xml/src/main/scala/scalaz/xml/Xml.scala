package scalaz
package xml

import cursor._, pp._

/**
 * Contains the entire XML library so as to provide a convenient single import for users.
 */
trait Xmls
extends Attrs
with Cs
with CDatas
with CDataKinds
with Contents
with Elements
with NSInfos
with Pps
with QNames
with Tokens
with Txts
with XSources
with IdentityXmls

/**
 * Contains the entire XML library so as to provide a convenient single import for users.
 */
object Xml extends Xmls
