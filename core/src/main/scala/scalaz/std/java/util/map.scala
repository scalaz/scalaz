package scalaz
package std.java.util

trait MapInstances {

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def mapEntryBitraverse: Bitraverse[Entry] = new Bitraverse[Entry] {
    override def bimap[A, B, C, D](fab: Entry[A, B])(f: A => C, g: B => D) =
      new SimpleImmutableEntry(f(fab.getKey), g(fab.getValue))

    def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: Entry[A, B])
                                                 (f: (A) => G[C], g: (B) => G[D]) =
      Applicative[G].apply(f(fab.getKey), g(fab.getValue))(new SimpleImmutableEntry(_, _))

  }
}

object map extends MapInstances
