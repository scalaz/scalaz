package scalaz.syntax

import scalaz._


final class TagOps[A, T] private[syntax](val self: A @@ T) extends AnyVal {
  def unwrap: A = Tag.unwrap(self)
}

trait ToTagOps {
  implicit def ToTagOps[A, T](tag: A @@ T): TagOps[A, T] = new TagOps(tag)
}
