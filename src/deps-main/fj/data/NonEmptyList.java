package fj.data;

import fj.Effect;
import fj.F;
import static fj.data.Option.some;

import java.util.Collection;
import java.util.Iterator;

/**
 * Provides an in-memory, immutable, singly linked list with total <code>head</code> and <code>tail</code>.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class NonEmptyList<A> implements Iterable<A> {
  /**
   * Returns an iterator for this non-empty list. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this non-empty list.
   */

  public Iterator<A> iterator() {
    return toCollection().iterator();
  }

  /**
   * The first element of this linked list.
   */
  @SuppressWarnings({"PublicField", "ClassEscapesDefinedScope"})
  public final A head;

  /**
   * This list without the first element.
   */
  @SuppressWarnings({"PublicField"})
  public final List<A> tail;

  private NonEmptyList(final A head, final List<A> tail) {
    this.head = head;
    this.tail = tail;
  }

  /**
   * Prepend the given value to this list.
   *
   * @param a The value to prepend.
   * @return A non-empty list with an extra element.
   */
  public NonEmptyList<A> cons(final A a) {
    return nel(a, tail.cons(head));
  }

  /**
   * Appends the given list to this list.
   *
   * @param as The list to append.
   * @return A new list with the given list appended.
   */
  public NonEmptyList<A> append(final NonEmptyList<A> as) {
    final List.Buffer<A> b = new List.Buffer<A>();
    b.append(tail);
    b.snoc(as.head);
    b.append(as.tail);
    final List<A> bb = b.toList();
    return nel(head, bb); 
  }

  /**
   * Maps the given function across this list.
   *
   * @param f The function to map across this list.
   * @return A new list after the given function has been applied to each element.
   */
  public <B> NonEmptyList<B> map(final F<A, B> f) {
    return nel(f.f(head), tail.map(f));
  }

  /**
   * Binds the given function across each element of this list with a final join.
   *
   * @param f The function to apply to each element of this list.
   * @return A new list after performing the map, then final join.
   */
  public <B> NonEmptyList<B> bind(final F<A, NonEmptyList<B>> f) {
    final List.Buffer<B> b = new List.Buffer<B>();
    final NonEmptyList<B> p = f.f(head);
    b.snoc(p.head);
    b.append(p.tail);
    tail.foreach(new Effect<A>() {
      public void e(final A a) {
        final NonEmptyList<B> p = f.f(a);
        b.snoc(p.head);
        b.append(p.tail);
      }
    });
    final List<B> bb = b.toList();
    return nel(bb.head(), bb.tail());
  }

  /**
   * Returns a <code>List</code> projection of this list.
   *
   * @return A <code>List</code> projection of this list.
   */
  public List<A> toList() {
    return tail.cons(head);
  }

  /**
   * Projects an immutable collection of this non-empty list.
   *
   * @return An immutable collection of this non-empty list.
   */
  public Collection<A> toCollection() {
    return toList().toCollection();
  }

  /**
   * Returns a function that takes a non-empty list to a list.
   *
   * @return A function that takes a non-empty list to a list.
   */
  public static <A> F<NonEmptyList<A>, List<A>> toList_() {
    return new F<NonEmptyList<A>, List<A>>() {
      public List<A> f(final NonEmptyList<A> as) {
        return as.toList();
      }
    };
  }

  /**
   * Return a non-empty list with the given head and tail.
   *
   * @param head The first element of the new list.
   * @param tail The remaining elements of the new list.
   * @return A non-empty list with the given head and tail.
   */
  public static <A> NonEmptyList<A> nel(final A head, final List<A> tail) {
    return new NonEmptyList<A>(head, tail);
  }

  /**
   * Return a non-empty list with the given value.
   *
   * @param head The value in the non-empty list.
   * @return A non-empty list with the given value.
   */
  public static <A> NonEmptyList<A> nel(final A head) {
    return nel(head, List.<A>nil());
  }

  /**
   * Returns a function that puts an element into a non-empty list.
   *
   * @return A function that puts an element into a non-empty list.
   */
  public static <A> F<A, NonEmptyList<A>> nel() {
    return new F<A, NonEmptyList<A>>() {
      public NonEmptyList<A> f(final A a) {
        return nel(a);
      }
    };
  }

  /**
   * Returns a potential non-empty list from the given list. A non-value is returned if the given list is empty.
   *
   * @param as The list to construct a potential non-empty list with.
   * @return A potential non-empty list from the given list.
   */
  public static <A> Option<NonEmptyList<A>> fromList(final List<A> as) {
    return as.isEmpty() ?
            Option.<NonEmptyList<A>>none() :
            some(nel(as.head(), as.tail()));
  }
}
