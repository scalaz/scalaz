package fj.data;

import fj.Unit;
import static fj.Unit.unit;
import fj.pre.Equal;
import fj.pre.Hash;

import java.util.Collection;
import java.util.Iterator;

/**
 * A mutable hash set that guarantees uniqueness of its elements providing O(1) lookup.
 *
 * @see HashMap
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class HashSet<A> implements Iterable<A> {
  /**
   * Returns an iterator for this hash set. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this hash set.
   */
  public Iterator<A> iterator() {
    return toCollection().iterator();
  }

  private final HashMap<A, Unit> m;

  /**
   * Construct a hash set with the given equality and hashing strategy.
   *
   * @param e The equality strategy.
   * @param h The hashing strategy.
   */
  public HashSet(final Equal<A> e, final Hash<A> h) {
    m = new HashMap<A, Unit>(e, h);
  }

  /**
   * Construct a hash set with the given equality and hashing strategy.
   *
   * @param e The equality strategy.
   * @param h The hashing strategy.
   * @param initialCapacity The initial capacity.
   */
  public HashSet(final Equal<A> e, final Hash<A> h, final int initialCapacity) {
    m = new HashMap<A, Unit>(e, h, initialCapacity);
  }

  /**
   * Construct a hash set with the given equality and hashing strategy.
   *
   * @param e The equality strategy.
   * @param h The hashing strategy.
   * @param initialCapacity The initial capacity.
   * @param loadFactor The load factor.
   */
  public HashSet(final Equal<A> e, final Hash<A> h, final int initialCapacity, final float loadFactor) {
    m = new HashMap<A, Unit>(e, h, initialCapacity, loadFactor);
  }

  /**
   * Compare two values for equality using the underlying equality strategy.
   *
   * @param a1 One value to compare.
   * @param a2 The other value to compare.
   * @return <code>true</code> if the two values are equal, <code>false</code> otherwise.
   */
  public boolean eq(final A a1, final A a2) {
    return m.eq(a1, a2);
  }

  /**
   * Compute the hash of the given value using the underlying hashing strategy.
   *
   * @param a The value to computer the hash of.
   * @return The hash of the given value.
   */
  public int hash(final A a) {
    return m.hash(a);
  }

  /**
   * Determines if this hash set contains the given element.
   *
   * @param a The element to look for in this hash set.
   * @return <code>true</code> if this hash set contains the given element, <code>false</code> otherwise.
   */
  public boolean contains(final A a) {
    return m.contains(a);
  }
  
  /**
   * Insert the given element into this hash set.
   *
   * @param a The element to insert.
   */
  public void set(final A a) {
    m.set(a, unit());
  }

  /**
   * Clear all elements from this hash set.
   */
  public void clear() {
    m.clear();
  }

  /**
   * Determines if this hash set contains any elements.
   *
   * @return <code>true</code> if this hash set contains no elements, <code>false</code> otherwise.
   */
  public boolean isEmpty() {
    return m.isEmpty();
  }

  /**
   * Returns the number of entries in this hash set.
   *
   * @return The number of entries in this hash set.
   */
  public int size() {
    return m.size();
  }

  /**
   * Deletes the given element from this hash set.
   *
   * @param a The element to delete from this hash set.
   * @return <code>true</code> if this hash set contained the given element prior to deletion, <code>false</code>
   * otherwise.
   */
  public boolean delete(final A a) {
    return m.getDelete(a).isSome();
  }

  /**
   * Returns a list projection of this hash set.
   *
   * @return A list projection of this hash set.
   */
  public List<A> toList() {
    return m.keys();
  }

  /**
   * Projects an immutable collection of this hash set.
   *
   * @return An immutable collection of this hash set.
   */
  public Collection<A> toCollection() {
    return toList().toCollection();
  }
}
