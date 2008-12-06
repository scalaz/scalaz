package fj.data;

import fj.F;
import fj.F2;
import static fj.Function.curry;
import fj.P2;
import static fj.data.List.cons;
import static fj.data.List.single;
import static fj.data.List.iterateWhile;
import fj.pre.Monoid;

import java.util.Collection;
import java.util.Iterator;

/**
 * Provides an immutable, non-empty, multi-way tree (a rose tree).
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>Author: runar</li>
 *          </ul>
 */
public final class Tree<A> implements Iterable<A> {
  /**
   * Returns an iterator for this tree. This method exists to permit the use in a <code>for</code>-each loop.
   *
   * @return A iterator for this tree.
   */
  public Iterator<A> iterator() {
    return toCollection().iterator();
  }

  private final A root;
  private final List<Tree<A>> subForest;

  private Tree(final A root, final List<Tree<A>> subForest) {
    this.root = root;
    this.subForest = subForest;
  }

  /**
   * Creates a nullary tree.
   *
   * @param root The root element of the tree.
   * @return A nullary tree with the root element in it.
   */
  public static <A> Tree<A> leaf(final A root) {
    return node(root, List.<Tree<A>>nil());
  }

  /**
   * Creates a new n-ary tree given a root and a subforest of length n.
   *
   * @param root   The root element of the tree.
   * @param forest A list of the tree's subtrees.
   * @return A newly sprouted tree.
   */
  public static <A> Tree<A> node(final A root, final List<Tree<A>> forest) {
    return new Tree<A>(root, forest);
  }

  /**
   * Returns the root element of the tree.
   *
   * @return The root element of the tree.
   */
  public A root() {
    return root;
  }

  /**
   * Returns a list of the tree's subtrees.
   *
   * @return A list of the tree's subtrees.
   */
  public List<Tree<A>> subForest() {
    return subForest;
  }

  /**
   * Provides a transformation from a tree to its root.
   *
   * @return A transformation from a tree to its root.
   */
  public static <A> F<Tree<A>, A> root_() {
    return new F<Tree<A>, A>() {
      public A f(final Tree<A> a) {
        return a.root();
      }
    };
  }

  /**
   * Provides a transformation from a tree to its subforest.
   *
   * @return A transformation from a tree to its subforest.
   */
  public static <A> F<Tree<A>, List<Tree<A>>> subForest_() {
    return new F<Tree<A>, List<Tree<A>>>() {
      public List<Tree<A>> f(final Tree<A> a) {
        return a.subForest();
      }
    };
  }

  /**
   * Puts the elements of the tree into a List, in pre-order.
   *
   * @return The elements of the tree in pre-order.
   */
  public List<A> flatten() {
    final F2<Tree<A>, List<A>, List<A>> squish = new F2<Tree<A>, List<A>, List<A>>() {
      public List<A> f(final Tree<A> t, final List<A> xs) {
        return cons(t.root(), t.subForest().foldRight(curry(this), xs));
      }
    };
    return squish.f(this, List.<A>nil());
  }

  /**
   * Puts the elements of the tree into a List, in pre-order.
   *
   * @return The elements of the tree in pre-order.
   */
  public static <A> F<Tree<A>, List<A>> flatten_() {
    return new F<Tree<A>, List<A>>() {
      public List<A> f(final Tree<A> t) {
        return t.flatten();
      }
    };
  }
   
  /**
   * Provides a list of the elements of the tree at each level, in level order.
   *
   * @return The elements of the tree at each level.
   */
  public List<List<A>> levels() {
    final F<List<Tree<A>>, List<Tree<A>>> flatSubForests = List.<Tree<A>, Tree<A>>bind_().f(Tree.<A>subForest_());
    final F<List<Tree<A>>, List<A>> roots = List.<Tree<A>, A>map_().f(Tree.<A>root_());
    return iterateWhile(flatSubForests, List.<Tree<A>>isNotEmpty_(), single(this)).map(roots);
  }

  /**
   * Maps the given function over this tree.
   *
   * @param f The function to map over this tree.
   * @return The new Tree after the function has been applied to each element in this Tree.
   */
  public <B> Tree<B> fmap(final F<A, B> f) {
    return node(f.f(root()), subForest().map(Tree.<A, B>fmap_().f(f)));
  }

  /**
   * Provides a transformation to lift any function so that it maps over Trees.
   *
   * @return A transformation to lift any function so that it maps over Trees.
   */
  public static <A, B> F<F<A, B>, F<Tree<A>, Tree<B>>> fmap_() {
    return new F<F<A, B>, F<Tree<A>, Tree<B>>>() {
      public F<Tree<A>, Tree<B>> f(final F<A, B> f) {
        return new F<Tree<A>, Tree<B>>() {
          public Tree<B> f(final Tree<A> a) {
            return a.fmap(f);
          }
        };
      }
    };
  }

  /**
   * Folds this tree using the given monoid.
   *
   * @param f A transformation from this tree's elements, to the monoid.
   * @param m The monoid to fold this tree with.
   * @return The result of folding the tree with the given monoid.
   */
  public <B> B foldMap(final F<A, B> f, final Monoid<B> m) {
    return m.sum(f.f(root()), m.sumRight(subForest().map(foldMap_(f, m))));
  }

  /**
   * Projects an immutable collection of this tree.
   *
   * @return An immutable collection of this tree.
   */
  public Collection<A> toCollection() {
    return flatten().toCollection();
  }

  /**
   * Provides a function that folds a tree with the given monoid.
   *
   * @param f A transformation from a tree's elements to the monoid.
   * @param m A monoid to fold the tree with.
   * @return A function that, given a tree, folds it with the given monoid.
   */
  public static <A, B> F<Tree<A>, B> foldMap_(final F<A, B> f, final Monoid<B> m) {
    return new F<Tree<A>, B>() {
      public B f(final Tree<A> t) {
        return t.foldMap(f, m);
      }
    };
  }

  /**
   * Build a tree from a seed value.
   *
   * @param f A function with which to build the tree.
   * @return A function which, given a seed value, yields a tree.
   */
  public static <A, B> F<B, Tree<A>> unfoldTree(final F<B, P2<A, List<B>>> f) {
    return new F<B, Tree<A>>() {
      public Tree<A> f(final B b) {
        final P2<A, List<B>> p = f.f(b);
        return node(p._1(), List.<B, Tree<A>>map_().f(unfoldTree(f)).f(p._2()));
      }
    };
  }

}