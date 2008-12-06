package fj.test.reflect;

import static fj.Bottom.error;
import fj.Class;
import static fj.Class.clas;
import fj.F;
import fj.Function;
import fj.P;
import static fj.P.p;
import fj.P2;
import fj.P3;
import fj.data.Array;
import static fj.data.Array.array;
import fj.data.List;
import static fj.data.List.join;
import static fj.data.List.list;
import fj.data.Option;
import static fj.data.Option.fromNull;
import static fj.data.Option.none;
import static fj.data.Option.some;
import static fj.data.Option.somes;
import fj.test.CheckResult;
import fj.test.Property;
import fj.test.Rand;

import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import static java.lang.reflect.Modifier.isStatic;

/**
 * Functions for checking properties in a class that are found reflectively and according to various
 * annotations.
 * 
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
public final class Check {
  private Check() {
    throw new UnsupportedOperationException();
  }

  /**
   * Returns the results and names of checking the properties on the given classes using a
   * {@link Rand#standard standard random generator}.
   *
   * @param c The classes to check the properties of.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results and names of checking the properties on the given classes using a
   * {@link Rand#standard standard random generator}.
   */
  public static <T> List<P2<String, CheckResult>> check(final List<java.lang.Class<T>> c, final String... categories) {
    return check(c, Rand.standard, categories);
  }

  /**
   * Returns the results and names of checking the properties on the given classes using a
   * {@link Rand#standard standard random generator}.
   *
   * @param c The classes to check the properties of.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results and names of checking the properties on the given classes using a
   * {@link Rand#standard standard random generator}.
   */
  public static <T> List<P2<String, CheckResult>> check(final List<java.lang.Class<T>> c, final List<String> categories) {
    return check(c, Rand.standard, categories.toArray().array());
  }

  /**
   * Returns the results and names of checking the properties on the given classes.
   *
   * @param c The classes to check the properties of.
   * @param r The random generator to use to check the properties on the given classes.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results and names of checking the properties on the given classes.
   */
  public static <T> List<P2<String, CheckResult>> check(final List<java.lang.Class<T>> c, final Rand r, final String... categories) {
    return join(c.map(new F<java.lang.Class<T>, List<P2<String, CheckResult>>>() {
      public List<P2<String, CheckResult>> f(final java.lang.Class<T> c) {
        return check(c, r, categories);
      }
    }));
  }

  /**
   * Returns the results and names of checking the properties on the given classes.
   *
   * @param c The classes to check the properties of.
   * @param r The random generator to use to check the properties on the given classes.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results and names of checking the properties on the given classes.
   */
  public static <T> List<P2<String, CheckResult>> check(final List<java.lang.Class<T>> c, final Rand r, final List<String> categories) {
    return check(c, r, categories.toArray().array());
  }

  /**
   * Returns the results and names of checking the properties on the given class using a
   * {@link Rand#standard standard random generator}.
   *
   * @param c The class to check the properties of.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results and names of checking the properties on the given class using a
   * {@link Rand#standard standard random generator}.
   */
  public static <T> List<P2<String, CheckResult>> check(final java.lang.Class<T> c, final String... categories) {
    return check(c, Rand.standard, categories);
  }

  /**
   * Returns the results and names of checking the properties on the given class using a
   * {@link Rand#standard standard random generator}.
   *
   * @param c The class to check the properties of.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results and names of checking the properties on the given class using a
   * {@link Rand#standard standard random generator}.
   */
  public static <T> List<P2<String, CheckResult>> check(final java.lang.Class<T> c, final List<String> categories) {
    return check(c, Rand.standard, categories.toArray().array());
  }

  /**
   * Returns the results and names of checking the properties on the given class.
   *
   * @param c The class to check the properties of.
   * @param r The random generator to use to check the properties on the given class.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results of checking the properties on the given class.
   */
  public static <T> List<P2<String, CheckResult>> check(final java.lang.Class<T> c, final Rand r, final String... categories) {
    return join(clas(c).inheritance().map(new F<Class<? super T>, List<P3<Property, String, Option<CheckParams>>>>() {
      public List<P3<Property, String, Option<CheckParams>>> f(final Class<? super T> c) {
        return properties(c.clas(), categories);
      }
    })).map(new F<P3<Property, String, Option<CheckParams>>, P2<String, CheckResult>>() {
      public P2<String, CheckResult> f(final P3<Property, String, Option<CheckParams>> p) {
        if(p._3().isSome()) {
          final CheckParams ps = p._3().some();
          return p(p._2(), p._1().check(r, ps.minSuccessful(), ps.maxDiscarded(), ps.minSize(), ps.maxSize()));
        } else
          return p(p._2(), p._1().check(r));
      }
    });
  }

  /**
   * Returns the results and names of checking the properties on the given class.
   *
   * @param c The class to check the properties of.
   * @param r The random generator to use to check the properties on the given class.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return The results of checking the properties on the given class.
   */
  public static <T> List<P2<String, CheckResult>> check(final java.lang.Class<T> c, final Rand r, final List<String> categories) {
    return check(c, r, categories.toArray().array());
  }

  /**
   * Returns all properties, their name and possible check parameters in a given class that are
   * found reflectively and according to various annotations. For example, properties or their
   * enclosing class that are annotated with {@link NoCheck} are not considered. The name of a
   * property is specified by the {@link Name annotation} or if this annotation is not present, the
   * name of the method or field that represents the property.
   *
   * @param c The class to look for properties on.
   * @param categories The categories of properties to return. If no categories are specified, all
   * candidate properties are returned, otherwise, only those properties in the given categories are
   * returned (properties in no category are omitted in this latter case).
   * @return All properties, their name and possible check parameters in a given class that are
   * found reflectively and according to various annotations.
   */
  public static <U, T extends U> List<P3<Property, String, Option<CheckParams>>> properties(final java.lang.Class<T> c, final String... categories) {
    //noinspection ClassEscapesDefinedScope
    final Array<P3<Property, String, Option<CheckParams>>> propFields = properties(array(c.getDeclaredFields()).map(new F<Field, PropertyMember>() {
      public PropertyMember f(final Field f) {
        return new PropertyMember() {
          public java.lang.Class<?> type() {
            return f.getType();
          }

          public AnnotatedElement element() {
            return f;
          }

          public String name() {
            return f.getName();
          }

          public int modifiers() {
            return f.getModifiers();
          }

          public <X> Property invoke(final X x) throws IllegalAccessException {
            f.setAccessible(true);
            return (Property)f.get(x);
          }

          public boolean isProperty() {
            return true;
          }
        };
      }
    }), c, categories);

    //noinspection ClassEscapesDefinedScope
    final Array<P3<Property, String, Option<CheckParams>>> propMethods = properties(array(c.getDeclaredMethods()).map(new F<Method, PropertyMember>() {
      public PropertyMember f(final Method m) {
        //noinspection ProhibitedExceptionDeclared
        return new PropertyMember() {
          public java.lang.Class<?> type() {
            return m.getReturnType();
          }

          public AnnotatedElement element() {
            return m;
          }

          public String name() {
            return m.getName();
          }

          public int modifiers() {
            return m.getModifiers();
          }

          public <X> Property invoke(final X x) throws Exception {
            m.setAccessible(true);
            return (Property)m.invoke(x);
          }

          public boolean isProperty() {
            return m.getParameterTypes().length == 0;
          }
        };
      }
    }), c, categories);

    return propFields.append(propMethods).toList();
  }

  private static interface PropertyMember {
    java.lang.Class<?> type();
    AnnotatedElement element();
    String name();
    int modifiers();
    @SuppressWarnings({"ProhibitedExceptionDeclared"})
    <X> Property invoke(X x) throws Exception;
    boolean isProperty();
  }

  private static <T> Array<P3<Property, String, Option<CheckParams>>> properties(final Array<PropertyMember> ms, final java.lang.Class<T> declaringClass, final String... categories) {
    final Option<T> t = emptyCtor(declaringClass).map(new F<Constructor<T>, T>() {
      public T f(final Constructor<T> ctor) {
        try {
          ctor.setAccessible(true);
          return ctor.newInstance();
        } catch(Exception e) {
          throw error(e.toString());
        }
      }
    });

    final F<AnnotatedElement, F<String, Boolean>> p = new F<AnnotatedElement, F<String, Boolean>>() {
      public F<String, Boolean> f(final AnnotatedElement e) {
        return new F<String, Boolean>() {
          public Boolean f(final String s) {
            final F<Category, Boolean> p = new F<Category, Boolean>() {
              public Boolean f(final Category c) {
                return array(c.value()).exists(new F<String, Boolean>() {
                  public Boolean f(final String cs) {
                    return cs.equals(s);
                  }
                });
              }
            };

            @SuppressWarnings("unchecked")
            final List<Boolean> bss = somes(list(fromNull(e.getAnnotation(Category.class)).map(p),
              fromNull(declaringClass.getAnnotation(Category.class)).map(p)));
            return bss.exists(Function.<Boolean>identity());
          }
        };
      }
    };

    final F<Name, String> nameS = new F<Name, String>() {
      public String f(final Name name) {
        return name.value();
      }
    };

    return ms.filter(new F<PropertyMember, Boolean>() {
      public Boolean f(final PropertyMember m) {
        return m.isProperty() &&
            m.type() == Property.class &&
            !m.element().isAnnotationPresent(NoCheck.class) &&
            !declaringClass.isAnnotationPresent(NoCheck.class) &&
            (categories.length == 0 || array(categories).exists(p.f(m.element()))) &&
            (t.isSome() || isStatic(m.modifiers()));
      }
    }).map(new F<PropertyMember, P3<Property, String, Option<CheckParams>>>() {
      public P3<Property, String, Option<CheckParams>> f(final PropertyMember m) {
        try {
          final Option<CheckParams> params = fromNull(m.element().getAnnotation(CheckParams.class)).orElse(fromNull(declaringClass.getAnnotation(CheckParams.class)));
          final String name = fromNull(m.element().getAnnotation(Name.class)).map(nameS).orSome(m.name());
          return p(m.invoke(t.orSome(P.<T>p(null))), name, params);
        } catch(Exception e) {
          throw error(e.toString());
        }
      }
    });
  }

  private static <T> Option<Constructor<T>> emptyCtor(final java.lang.Class<T> c) {
    Option<Constructor<T>> ctor;

    //noinspection UnusedCatchParameter
    try {
      ctor = some(c.getDeclaredConstructor());
    } catch(NoSuchMethodException e) {
      ctor = none();
    }
    return ctor;
  }
}
