package scalaz
package std.java.util

trait MapInstances {

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def mapEntryBiTraverse: BiTraverse[Entry] = new BiTraverse[Entry] {
    override def bimap[A, B, C, D](fab: Entry[A, B])(f: A => C, g: B => D) =
      new SimpleImmutableEntry(f(fab.getKey), g(fab.getValue))

    def bitraverse[G[_]: Applicative, A, B, C, D](fab: Entry[A, B])
                                                 (f: (A) => G[C], g: (B) => G[D]) =
      Applicative[G].map2(f(fab.getKey), g(fab.getValue))(new SimpleImmutableEntry(_, _))

  }
}

object map extends MapInstances
