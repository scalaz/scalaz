package scalaz

/**
 * Mark an explicit or implicit parameter as known to be unused, as shorthand
 * for the longer form, useful in combination with
 * `-Ywarn-unused:explicits,implicits` compiler options.
 */
class unused extends deprecated("unused", "")

