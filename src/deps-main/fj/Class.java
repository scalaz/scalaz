package fj;

import fj.data.List;
import static fj.data.List.unfold;
import fj.data.Option;
import static fj.data.Option.none;
import static fj.data.Option.some;
import fj.data.Tree;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;

/**
 * A wrapper for a {@link java.lang.Class} that provides additional methods.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Class<T> {
  private final java.lang.Class<T> c;

  private Class(final java.lang.Class<T> c) {
    this.c = c;
  }

  /**
   * Returns the inheritance hierarchy of this class.
   *
   * @return The inheritance hierarchy of this class.
   */
  public List<Class<? super T>> inheritance() {
    return unfold(new F<java.lang.Class<? super T>, Option<P2<java.lang.Class<? super T>, java.lang.Class<? super T>>>>() {
      public Option<P2<java.lang.Class<? super T>, java.lang.Class<? super T>>> f(final java.lang.Class<? super T> c) {
        if (c == null)
          return none();
        else {
          final P2<java.lang.Class<? super T>, java.lang.Class<? super T>> p = new P2<java.lang.Class<? super T>, java.lang.Class<? super T>>() {
            public java.lang.Class<? super T> _1() {
              return c;
            }

            @SuppressWarnings({"unchecked"})
            public java.lang.Class<? super T> _2() {
              return c.getSuperclass();
            }
          };
          return some(p);
        }
      }
    }, c).map(new F<java.lang.Class<? super T>, Class<? super T>>() {
      public Class<? super T> f(final java.lang.Class<? super T> c) {
        return clas(c);
      }
    });
  }

  /**
   * Provides this class's type parameter information as a Tree of the type expression.
   * Only descends into Parameterized classes. Non-abstract classes, or classes that don't implement an interface,
   * are treated as raw types. Arrays, Type Variables, and Wildcards are treated as opaque Types.
   *
   * @return The rose tree representing the type expression for this class.
   */
  public Tree<Type> classParameters() {
    return typeParameterTree(c);
  }

  /**
   * Provides this class's superclass type parameter information as a Tree of the type expression.
   * Only descends into Parameterized classes. Non-abstract classes, or classes that don't implement an interface,
   * are treated as raw types. Arrays, Type Variables, and Wildcards are treated as opaque Types.
   *
   * @return The Tree representing the type expression for this class's superclass.
   */
  public Tree<Type> superclassParameters() {
    return typeParameterTree(c.getGenericSuperclass());
  }

  /**
   * Provides this class's interface type parameter information as a list of trees.
   * @return A list of trees representing the type expressions for this class's interfaces. 
   */
  public List<Tree<Type>> interfaceParameters() {
    List<Tree<Type>> ts = List.nil();
    for (final Type t : c.getInterfaces()) {
      ts = ts.snoc(typeParameterTree(t));
    }
    return ts;
  }

  /**
   * Provides type parameter information as a Tree of the type expression.
   * Only descends into Parameterized classes. Non-abstract classes, or classes that don't implement an interface,
   * are treated as raw types. Arrays, Type Variables, and Wildcards are treated as opaque Types.
   *
   * @param t The type (class) for which to get the generic type information.
   * @return Type parameter information as a rose tree of the type expression.
   */
  public static Tree<Type> typeParameterTree(final Type t) {
    List<Tree<Type>> typeArgs = List.nil();
    final Tree<Type> types;
    if (t instanceof ParameterizedType) {
      final ParameterizedType pt = (ParameterizedType) t;
      for (final Type arg : pt.getActualTypeArguments()) {
        typeArgs = typeArgs.snoc(typeParameterTree(arg));
      }
      types = Tree.node(pt.getRawType(), typeArgs);
    } else {
      types = Tree.node(t, List.<Tree<Type>>nil());
    }
    return types;
  }

  /**
   * Returns the underlying class.
   *
   * @return The underlying class.
   */
  public java.lang.Class<T> clas() {
    return c;
  }

  /**
   * Constructs a class from the given argument.
   *
   * @param c The argument to construct this class with.
   * @return A class from the given argument.
   */
  public static <T> Class<T> clas(final java.lang.Class<T> c) {
    return new Class<T>(c);
  }
}
