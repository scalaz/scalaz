package fj.data;

import fj.pre.Ord;
import fj.pre.Monoid;
import fj.pre.Ordering;
import static fj.pre.Ordering.LT;
import static fj.pre.Ordering.GT;
import fj.F;
import fj.F2;
import fj.P;
import fj.P3;
import static fj.data.Option.some;
import static fj.function.Booleans.not;
import static fj.Function.identity;
import static fj.Function.curry;
import static fj.Function.compose;
import static fj.Function.constant;

import java.util.Iterator;

/**
 * Provides an in-memory, immutable set, implemented as a red/black tree.
 */
public abstract class Set<A> implements Iterable<A> {
  private Set(final Ord<A> ord) {
    this.ord = ord;
  }

  private enum Color {
    R, B
  }

  private final Ord<A> ord;

  public boolean isEmpty() {
    return this instanceof Empty;
  }

  @SuppressWarnings({"ClassEscapesDefinedScope"})
  abstract Color color();

  abstract Set<A> l();

  abstract A head();

  abstract Set<A> r();

  private static final class Empty<A> extends Set<A> {
    private Empty(final Ord<A> ord) {
      super(ord);
    }

    public Color color() {
      return Color.B;
    }

    public Set<A> l() {
      throw new Error("Left on empty set.");
    }

    public Set<A> r() {
      throw new Error("Right on empty set.");
    }

    public A head() {
      throw new Error("Head on empty set.");
    }
  }

  private static final class Tree<A> extends Set<A> {
    private final Color c;
    private final Set<A> a;
    private final A x;
    private final Set<A> b;

    private Tree(final Ord<A> ord, final Color c, final Set<A> a, final A x, final Set<A> b) {
      super(ord);
      this.c = c;
      this.a = a;
      this.x = x;
      this.b = b;
    }

    public Color color() {
      return c;
    }

    public Set<A> l() {
      return a;
    }

    public A head() {
      return x;
    }

    public Set<A> r() {
      return b;
    }
  }

  /**
   * The empty set.
   *
   * @param ord An order for the type of elements.
   * @return the empty set.
   */
  public static <A> Set<A> empty(final Ord<A> ord) {
    return new Empty<A>(ord);
  }

  /**
   * Checks if the given element is a member of this set.
   *
   * @param x An element to check for membership in this set.
   * @return true if the given element is a member of this set.
   */
  public boolean member(final A x) {
    return !isEmpty() && (ord.isLessThan(x, head()) && l().member(x) || ord.eq(head(), x) || r().member(x));
  }


  /**
   * First-class membership check.
   *
   * @return A function that returns true if the given element if a member of the given set.
   */
  public F<Set<A>, F<A, Boolean>> member() {
    return curry(new F2<Set<A>, A, Boolean>() {
      public Boolean f(final Set<A> s, final A a) {
        return s.member(a);
      }
    });
  }
  
  /**
   * Inserts the given element into this set.
   *
   * @param x An element to insert into this set.
   * @return A new set with the given element inserted.
   */
  public Set<A> insert(final A x) {
    return ins(x).makeBlack();
  }

  private Set<A> ins(final A x) {
    return isEmpty() ?
      new Tree<A>(ord, Color.R, empty(ord), x, empty(ord)) :
      ord.isLessThan(x, head()) ?
        balance(ord, color(), l().ins(x), head(), r()) :
        ord.eq(x, head()) ?
          this :
          balance(ord, color(), l(), head(), r().ins(x));
  }

  private Set<A> makeBlack() {
    return new Tree<A>(ord, Color.B, l(), head(), r());
  }

  private static <A> Tree<A> tr(final Ord<A> o,
                                final Set<A> a, final A x, final Set<A> b,
                                final A y,
                                final Set<A> c, final A z, final Set<A> d) {
    return new Tree<A>(o, Color.R, new Tree<A>(o, Color.B, a, x, b), y, new Tree<A>(o, Color.B, c, z, d));
  }

  private static <A> Set<A> balance(final Ord<A> ord, final Color c, final Set<A> l, final A h, final Set<A> r) {
    if (c == Color.B && l.isTR() && l.l().isTR()) {
      return tr(ord, l.l().l(), l.l().head(), l.l().r(), l.head(), l.r(), h, r);
    } else if (c == Color.B && l.isTR() && l.r().isTR()) {
      return tr(ord, l.l(), l.head(), l.r().l(), l.r().head(), l.r().r(), h, r);
    } else if (c == Color.B && r.isTR() && r.l().isTR()) {
      return tr(ord, l, h, r.l().l(), r.l().head(), r.l().r(), r.head(), r.r());
    } else if (c == Color.B && r.isTR() && r.r().isTR()) {
      return tr(ord, l, h, r.l(), r.head(), r.r().l(), r.r().head(), r.r().r());
    } else {
      return new Tree<A>(ord, c, l, h, r);
    }
  }

  private boolean isTR() {
    return !isEmpty() && color() == Color.R;
  }

  /**
   * Returns an iterator over this set.
   *
   * @return an iterator over this set.
   */
  public Iterator<A> iterator() {
    return toStream().iterator();
  }

  /**
   * Returns a set with a single element.
   *
   * @param o An order for the type of element.
   * @param a An element to put in a set.
   * @return A new set with the given element in it.
   */
  public static <A> Set<A> single(final Ord<A> o, final A a) {
    return empty(o).insert(a);
  }

  /**
   * Maps the given function across this set.
   *
   * @param o An order for the elements of the new set.
   * @param f A function to map across this set.
   * @return The set of the results of applying the given function to the elements of this set.
   */
  public <B> Set<B> map(final Ord<B> o, final F<A, B> f) {
    return iterableSet(o, toStream().map(f));
  }

  /**
   * Folds this Set using the given monoid.
   *
   * @param f A transformation from this Set's elements, to the monoid.
   * @param m The monoid to fold this Set with.
   * @return The result of folding the Set with the given monoid.
   */
  public <B> B foldMap(final F<A, B> f, final Monoid<B> m) {
    return isEmpty() ?
      m.zero() :
      m.sum(m.sum(r().foldMap(f, m), f.f(head())), l().foldMap(f, m));
  }

  /**
   * Returns a list representation of this set.
   *
   * @return a list representation of this set.
   */
  public List<A> toList() {
    return foldMap(List.cons(List.<A>nil()), Monoid.<A>listMonoid());
  }

  /**
   * Returns a stream representation of this set.
   *
   * @return a stream representation of this set.
   */
  public Stream<A> toStream() {
    return foldMap(Stream.<A>single(), Monoid.<A>streamMonoid());
  }

  /**
   * Binds the given function across this set.
   *
   * @param o An order for the elements of the target set.
   * @param f A function to bind across this set.
   * @return A new set after applying the given function and joining the resulting sets.
   */
  public <B> Set<B> bind(final Ord<B> o, final F<A, Set<B>> f) {
    return join(o, map(Ord.setOrd(o), f));
  }

  /**
   * Add all the elements of the given set to this set.
   *
   * @param s A set to add to this set.
   * @return A new set containing all elements of both sets.
   */
  public Set<A> union(final Set<A> s) {
    return iterableSet(ord, s.toStream().append(toStream()));
  }

  /**
   * Filters elements from this set by returning only elements which produce <code>true</code>
   * when the given function is applied to them.
   *
   * @param f The predicate function to filter on.
   * @return A new set whose elements all match the given predicate.
   */
  public Set<A> filter(final F<A, Boolean> f) {
    return iterableSet(ord, toStream().filter(f));
  }

  /**
   * Deletes the given element from this set.
   *
   * @param a an element to remove.
   * @return A new set containing all the elements of this set, except the given element.
   */
  public Set<A> delete(final A a) {
    return minus(single(ord, a));
  }

  /**
   * Remove all elements from this set that do not occur in the given set.
   *
   * @param s A set of elements to retain.
   * @return A new set which is the intersection of this set and the given set.
   */
  public Set<A> intersect(final Set<A> s) {
    return filter(member().f(s));
  }

  /**
   * Remove all elements from this set that occur in the given set.
   *
   * @param s A set of elements to delete.
   * @return A new set which contains only the elements of this set that do not occur in the given set.
   */
  public Set<A> minus(final Set<A> s) {
    return filter(compose(not, member().f(s)));
  }

  /**
   * Returns the size of this set.
   *
   * @return The number of elements in this set.
   */
  public int size() {
    final F<A, Integer> one = constant(1);
    return foldMap(one, Monoid.intAdditionMonoid);
  }

  /**
   * Returns a product-3 of:
   * <ul>
   * <li>A set containing all the elements of this set which are less than the given value.</li>
   * <li>An option of a value equal to the given value, if one was found in this set, otherwise None.
   * <li>A set containing all the elements of this set which are greater than the given value.</li>
   * </ul>
   *
   * @param a A value at which to split this set.
   * @return A pair of sets where all elements in the first set are are less than the given value
   *         and all the elements in the second set are greater than the given value.
   */
  public P3<Set<A>, Option<A>, Set<A>> split(final A a) {
    if (isEmpty())
      return P.p(empty(ord), Option.<A>none(), empty(ord));
    else {
      final Ordering i = ord.compare(a, head());
      if (i == LT) {
        final P3<Set<A>, Option<A>, Set<A>> lg = l().split(a);
        return P.p(lg._1(), lg._2(), lg._3().insert(head()).union(r()));
      } else if (i == GT) {
        final P3<Set<A>, Option<A>, Set<A>> lg = r().split(a);
        return P.p(lg._1().insert(head()).union(l()), lg._2(), lg._3());
      } else
        return P.p(l(), some(a), r());
    }
  }

  /**
   * Returns true if this set is a subset of the given set.
   *
   * @param s A set which is a superset of this set if this method returns true.
   * @return true if this set is a subset of the given set.
   */
  public boolean subsetOf(final Set<A> s) {
    if (isEmpty() || s.isEmpty())
      return isEmpty();
    else {
      final P3<Set<A>, Option<A>, Set<A>> find = s.split(head());
      return find._2().isSome() && l().subsetOf(find._1()) && r().subsetOf(find._3());
    }
  }

  /**
   * Join a set of sets into a single set.
   *
   * @param s A set of sets.
   * @param o An order for the elements of the new set.
   * @return A new set which is the join of the given set of sets.
   */
  public static <A> Set<A> join(final Ord<A> o, final Set<Set<A>> s) {
    final F<Set<A>, Set<A>> id = identity();
    return s.foldMap(id, Monoid.<A>setMonoid(o));
  }

  /**
   * Return the elements of the given iterable as a set.
   *
   * @param o  An order for the elements of the new set.
   * @param as An iterable of elements to add to a set.
   * @return A new set containing the elements of the given iterable.
   */
  public static <A> Set<A> iterableSet(final Ord<A> o, final Iterable<A> as) {
    Set<A> s = empty(o);
    for (final A a : as)
      s = s.insert(a);
    return s;
  }

}
