package scalaz

/**
 * Mark an explicit or implicit parameter as known to be unused, as shorthand
 * for the longer form, useful in combination with
 * `-Ywarn-unused:explicits,implicits` compiler options.
 */
@deprecated("Will be removed", "7.3.9")
class unused extends deprecated("unused", "")
