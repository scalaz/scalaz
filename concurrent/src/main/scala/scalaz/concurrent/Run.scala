package scalaz
package concurrent


trait Run[F]  { self =>

  def run(a: F): Unit

  def strategy: Strategy

  def !(a: F): () => Unit = strategy(run(a))

  // derived functions
  def contramap[B](f: B => F): Run[B] = new Run[B] {
    def run(a: B) = self.run(f(a))
    def strategy: Strategy = self.strategy
  }
}

object Run {
  def apply[A](c: A => Unit)(implicit s: Strategy): Run[A] = new Run[A] {
    def run(a: A) = c(a)
    val strategy = s
  }

  implicit def RunFrom[A](e: Run[A]): A => Unit = e.run _

  implicit val runContravariant = new Contravariant[Run] {
    def contramap[A, B](r: Run[A])(f: B => A): Run[B] = new Run[B] {

      def strategy: Strategy = r.strategy

      def run(a: B) {
        r.run(f(a))
      }
    }
  }

  ////
}

