package scalaz

trait ~>[F[_],G[_]] { self =>  
  def apply[A](fa: F[A]): G[A]
  def compose[E[_]](f: E ~> F): E ~> G = new (E ~> G) {
    def apply[A](ea: E[A]) = self(f(ea))
  }
}
