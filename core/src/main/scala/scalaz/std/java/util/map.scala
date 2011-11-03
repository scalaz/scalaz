package scalaz
package std.java.util

trait MapInstances {

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def mapEntryBiFunctor: BiFunctor[Entry] = new BiFunctor[Entry] {
    def bimap[A, B, C, D](fab: Entry[A, B])(f: A => C, g: B => D) =
      new SimpleImmutableEntry(f(fab.getKey), g(fab.getValue))
  }
}

object map extends MapInstances