package fj.test;

import fj.Effect;
import fj.F;
import static fj.P.p;
import static fj.P.p2;
import static fj.P.p3;
import static fj.P.p4;
import static fj.P.p5;
import static fj.P.p6;
import static fj.P.p7;
import static fj.P.p8;
import fj.P1;
import fj.P2;
import fj.P3;
import fj.P4;
import fj.P5;
import fj.P6;
import fj.P7;
import fj.P8;
import static fj.Primitive.Byte_Long;
import static fj.Primitive.Character_Long;
import static fj.Primitive.Double_Long;
import static fj.Primitive.Float_Long;
import static fj.Primitive.Integer_Long;
import static fj.Primitive.Long_Byte;
import static fj.Primitive.Long_Character;
import static fj.Primitive.Long_Double;
import static fj.Primitive.Long_Float;
import static fj.Primitive.Long_Integer;
import static fj.Primitive.Long_Short;
import static fj.Primitive.Short_Long;
import fj.data.Array;
import static fj.data.Array.array;
import fj.data.Either;
import fj.data.Function;
import fj.data.Java;
import fj.data.List;
import static fj.data.List.isNotEmpty_;
import fj.data.Option;
import fj.data.Stream;
import static fj.data.Stream.cons;
import static fj.data.Stream.iterate;
import static fj.data.Stream.nil;

import static java.lang.System.arraycopy;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.Time;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Calendar;
import java.util.Date;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.IdentityHashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.PriorityQueue;
import java.util.Properties;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;
import java.util.WeakHashMap;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.concurrent.DelayQueue;
import java.util.concurrent.Delayed;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.SynchronousQueue;

/**
 * Represents a shrinking strategy over the given type parameter if that type can be represented as
 * a tree structure. This is used in falsification to produce the smallest counter-example, rather
 * than the first counter-example.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
public final class Shrink<A> {
  private final F<A, Stream<A>> f;

  private Shrink(final F<A, Stream<A>> f) {
    this.f = f;
  }

  /**
   * Returns a shrink of the given argument.
   *
   * @param a The argument to shrink.
   * @return A shrink of the given argument.
   */
  public Stream<A> shrink(final A a) {
    return f.f(a);
  }

  /**
   * Creates a shrink from this shrink and the given symmetric transformations.
   *
   * @param f A transformation from this shrink type to the new shrink type.
   * @param g A transformation from the new shrink type to this shrink type.
   * @return A shrink from this shrink and the given symmetric transformations.
   */
  public <B> Shrink<B> map(final F<A, B> f, final F<B, A> g) {
      return new Shrink<B>(new F<B, Stream<B>>() {
        public Stream<B> f(final B b) {
          return Shrink.this.f.f(g.f(b)).map(f);
        }
      });
    }
  

  /**
   * Constructs a shrink strategy from the given function that produces a tree of values given a
   * value.
   *
   * @param f A function that produces a tree of values given a value.
   * @return A shrink strategy from the given function that produces a tree of values given a
   * value.
   */
  public static <A> Shrink<A> shrink(final F<A, Stream<A>> f) {
    return new Shrink<A>(f);
  }

  /**
   * Returns a shrink strategy that cannot be reduced further.
   *
   * @return A shrink strategy that cannot be reduced further.
   */
  public static <A> Shrink<A> empty() {
    return new Shrink<A>(new F<A, Stream<A>>() {
      public Stream<A> f(final A a) {
        return nil();
      }
    });
  }

  /**
   * A shrink strategy for longs using 0 as the bottom of the shrink.
   */
  public static final Shrink<Long> shrinkLong = new Shrink<Long>(new F<Long, Stream<Long>>() {
      public Stream<Long> f(final Long i) {
        if(i == 0L)
          return nil();
        else {
          final Stream<Long> is = cons(0L, new P1<Stream<Long>>() {
            public Stream<Long> _1() {
              return iterate(new F<Long, Long>() {
                public Long f(final Long x) {
                  return x / 2L;
                }
              }, i).takeWhile(new F<Long, Boolean>() {
                public Boolean f(final Long x) {
                  return x != 0L;
                }
              }).map(new F<Long, Long>() {
                public Long f(final Long x) {
                  return i - x;
                }
              });
            }
          });

          return i < 0L ? cons(-i, new P1<Stream<Long>>() {
            public Stream<Long> _1() {
              return is;
            }
          }) : is;
        }
      }
    });

  /**
   * A shrink strategy for booleans using false as the bottom of the shrink.
   */
  public static final Shrink<Boolean> shrinkBoolean =
      shrink(fj.Function.<Boolean, Stream<Boolean>>constant(Stream.single(false)));

  /**
   * A shrink strategy for integers using 0 as the bottom of the shrink.
   */
  public static final Shrink<Integer> shrinkInteger = shrinkLong.map(Long_Integer, Integer_Long);

  /**
   * A shrink strategy for bytes using 0 as the bottom of the shrink.
   */
  public static final Shrink<Byte> shrinkByte = shrinkLong.map(Long_Byte, Byte_Long);

  /**
   * A shrink strategy for characters using 0 as the bottom of the shrink.
   */
  public static final Shrink<Character> shrinkCharacter = shrinkLong.map(Long_Character, Character_Long);

  /**
   * A shrink strategy for shorts using 0 as the bottom of the shrink.
   */
  public static final Shrink<Short> shrinkShort = shrinkLong.map(Long_Short, Short_Long);

  /**
   * A shrink strategy for floats using 0 as the bottom of the shrink.
   */
  public static final Shrink<Float> shrinkFloat = shrinkLong.map(Long_Float, Float_Long);

  /**
   * A shrink strategy for doubles using 0 as the bottom of the shrink.
   */
  public static final Shrink<Double> shrinkDouble = shrinkLong.map(Long_Double, Double_Long);

  /**
   * Returns a shrink strategy for optional values. A 'no value' is already fully
   * shrunk, otherwise, the shrinking occurs on the value with the given shrink strategy.
   *
   * @param sa The shrink strategy for the potential value.
   * @return A shrink strategy for optional values.
   */
  public static <A> Shrink<Option<A>> shrinkOption(final Shrink<A> sa) {
    return new Shrink<Option<A>>(new F<Option<A>, Stream<Option<A>>>() {
      public Stream<Option<A>> f(final Option<A> o) {
        return o.isNone() ?
            Stream.<Option<A>>nil() :
            cons(Option.<A>none(), new P1<Stream<Option<A>>>() {
              public Stream<Option<A>> _1() {
                return sa.shrink(o.some()).map(Option.<A>some_());
              }
            });
      }
    });
  }

  /**
   * Returns a shrink strategy for either values.
   *
   * @param sa The shrinking strategy for left values.
   * @param sb The shrinking strategy for right values.
   * @return A shrink strategy for either values.
   */
  public static <A, B> Shrink<Either<A, B>> shrinkEither(final Shrink<A> sa, final Shrink<B> sb) {
    return new Shrink<Either<A, B>>(new F<Either<A, B>, Stream<Either<A, B>>>() {
      public Stream<Either<A, B>> f(final Either<A, B> e) {
        return e.isLeft() ?
            sa.shrink(e.left().value()).map(Either.<A, B>left_()) :
            sb.shrink(e.right().value()).map(Either.<A, B>right_());        
      }
    });
  }

  /**
   * Returns a shrink strategy for lists. An empty list is fully shrunk.
   *
   * @param sa The shrink strategy for the elements of the list.
   * @return A shrink strategy for lists.
   */
  public static <A> Shrink<List<A>> shrinkList(final Shrink<A> sa) {
    final class Util {
      Stream<List<A>> removeChunks(final int n, final List<A> as) {
        if(as.isEmpty())
          return nil();
        else if(as.tail().isEmpty())
          return cons(List.<A>nil(), Stream.<List<A>>nil_());
        else {
          final int n1 = n / 2;
          final int n2 = n - n1;

          final List<A> as1 = as.take(n1);
          final F<List<A>, Boolean> isNotEmpty = isNotEmpty_();
          return cons(as1, new P1<Stream<List<A>>>() {
            public Stream<List<A>> _1() {
              final List<A> as2 = as.drop(n1);
              return cons(as2, new P1<Stream<List<A>>>() {
                public Stream<List<A>> _1() {
                  return removeChunks(n1, as1).filter(isNotEmpty).map(new F<List<A>, List<A>>() {
                      public List<A> f(final List<A> aas) {
                        return aas.append(as2);
                      }
                    }).interleave(removeChunks(n2, as2).filter(isNotEmpty).map(new F<List<A>, List<A>>() {
                    public List<A> f(final List<A> aas) {
                      return as1.append(aas);
                    }
                  }));
                }
              });
            }
          });
        }
      }

      Stream<List<A>> shrinkOne(final List<A> as) {
        if(as.isEmpty())
          return nil();
        else
         return sa.shrink(as.head()).map(new F<A, List<A>>() {
            public List<A> f(final A a) {
              return as.tail().cons(a);
            }
          }).append(shrinkOne(as.tail()).map(new F<List<A>, List<A>>() {
            public List<A> f(final List<A> aas) {
              return aas.cons(as.head());
            }
          }));        
      }
    }

    return new Shrink<List<A>>(new F<List<A>, Stream<List<A>>>() {
      public Stream<List<A>> f(final List<A> as) {
        final Util u = new Util();
        return u.removeChunks(as.length(), as).append(u.shrinkOne(as));
      }
    });
  }

  /**
   * Returns a shrink strategy for arrays. An empty array is fully shrunk.
   *
   * @param sa The shrink strategy for the elements of the array.
   * @return A shrink strategy for arrays.
   */
  public static <A> Shrink<Array<A>> shrinkArray(final Shrink<A> sa) {
    return shrinkList(sa).map(Function.<A>List_Array(), Function.<A>Array_List());
  }

  /**
   * Returns a shrink strategy for streams. An empty stream is fully shrunk.
   *
   * @param sa The shrink strategy for the elements of the stream.
   * @return A shrink strategy for streams.
   */
  public static <A> Shrink<Stream<A>> shrinkStream(final Shrink<A> sa) {
    return shrinkList(sa).map(Function.<A>List_Stream(), Function.<A>Stream_List());
  }

  /**
   * A shrink strategy for strings using the empty string as the bottom of the shrink.
   */
  public static final Shrink<String> shrinkString =
      shrinkList(shrinkCharacter).map(Function.List_String, Function.String_List);

  /**
   * A shrink strategy for string buffers using the empty string as the bottom of the shrink.
   */
  public static final Shrink<StringBuffer> shrinkStringBuffer =
      shrinkList(shrinkCharacter).map(Function.List_StringBuffer, Function.StringBuffer_List);

  /**
   * A shrink strategy for string builders using the empty string as the bottom of the shrink.
   */
  public static final Shrink<StringBuilder> shrinkStringBuilder =
      shrinkList(shrinkCharacter).map(Function.List_StringBuilder, Function.StringBuilder_List);

  /**
   * A shrink strategy for throwables.
   *
   * @param ss A shrink strategy for throwable messages.
   * @return A shrink strategy for throwables.
   */
  public static Shrink<Throwable> shrinkThrowable(final Shrink<String> ss) {
    return ss.map(new F<String, Throwable>() {
      public Throwable f(final String s) {
        //noinspection ThrowableInstanceNeverThrown
        return new Throwable(s);
      }
    }, new F<Throwable, String>() {
      public String f(final Throwable t) {
        return t.getMessage();
      }
    });
  }

  /**
   * A shrink strategy for throwables.
   */
  public static final Shrink<Throwable> shrinkThrowable = shrinkThrowable(shrinkString);

  // BEGIN java.util

  /**
   * Returns a shrink strategy for array lists. An empty array list is fully shrunk.
   *
   * @param sa The shrink strategy for the elements of the array list.
   * @return A shrink strategy for array lists.
   */
  public static <A> Shrink<ArrayList<A>> shrinkArrayList(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_ArrayList(), Java.<A>ArrayList_List());
  }

  /**
   * A shrink strategy for bit sets.
   */
  public static final Shrink<BitSet> shrinkBitSet =
      shrinkList(shrinkBoolean).map(Java.List_BitSet, Java.BitSet_List);

  /**
   * A shrink strategy for calendars.
   */
  public static final Shrink<Calendar> shrinkCalendar =
      shrinkLong.map(new F<Long, Calendar>() {
        public Calendar f(final Long i) {
          final Calendar c = Calendar.getInstance();
          c.setTimeInMillis(i);
          return c;
        }
      }, new F<Calendar, Long>() {
        public Long f(final Calendar c) {
          return c.getTime().getTime();
        }
      });

  /**
   * A shrink strategy for dates.
   */
  public static final Shrink<Date> shrinkDate =
      shrinkLong.map(new F<Long, Date>() {
        public Date f(final Long i) {
          return new Date(i);
        }
      }, new F<Date, Long>() {
        public Long f(final Date d) {
          return d.getTime();
        }
      });

  /**
   * A shrink strategy for enum maps.
   * 
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for enum maps.
   */
  public static <K extends Enum<K>, V> Shrink<EnumMap<K, V>> shrinkEnumMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, EnumMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public EnumMap<K, V> f(final Hashtable<K, V> h) {
        return new EnumMap<K, V>(h);
      }
    }, new F<EnumMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final EnumMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  /**
   * A shrink strategy for enum sets.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for enum sets.
   */
  public static <A extends Enum<A>> Shrink<EnumSet<A>> shrinkEnumSet(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_EnumSet(), Java.<A>EnumSet_List());
  }

  /**
   * A shrink strategy for gregorian calendars.
   */
  public static final Shrink<GregorianCalendar> shrinkGregorianCalendar =
      shrinkLong.map(new F<Long, GregorianCalendar>() {
        public GregorianCalendar f(final Long i) {
          final GregorianCalendar c = new GregorianCalendar();
          c.setTimeInMillis(i);
          return c;
        }
      }, new F<GregorianCalendar, Long>() {
        public Long f(final GregorianCalendar c) {
          return c.getTime().getTime();
        }
      });

  /**
   * A shrink strategy for hash maps.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for hash maps.
   */
  public static <K, V> Shrink<HashMap<K, V>> shrinkHashMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, HashMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public HashMap<K, V> f(final Hashtable<K, V> h) {
        return new HashMap<K, V>(h);
      }
    }, new F<HashMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final HashMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  /**
   * A shrink strategy for hash sets.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for hash sets.
   */
  public static <A> Shrink<HashSet<A>> shrinkHashSet(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_HashSet(), Java.<A>HashSet_List());
  }

  /**
   * A shrink strategy for hash tables.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for hash tables.
   */
  @SuppressWarnings({"UseOfObsoleteCollectionType"})
  public static <K, V> Shrink<Hashtable<K, V>> shrinkHashtable(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkList(shrinkP2(sk, sv)).map(new F<List<P2<K, V>>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final List<P2<K, V>> kvs) {
        final Hashtable<K, V> h = new Hashtable<K, V>();
        kvs.foreach(new Effect<P2<K, V>>() {
          public void e(final P2<K, V> kv) {
            h.put(kv._1(), kv._2());
          }
        });
        return h;
      }
    }, new F<Hashtable<K, V>, List<P2<K, V>>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public List<P2<K, V>> f(final Hashtable<K, V> h) {
        List<P2<K, V>> x = List.nil();

        for(final K k : h.keySet()) {
          x = x.snoc(p(k, h.get(k)));
        }

        return x;
      }
    });
  }

  /**
   * A shrink strategy for identity hash maps.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for identity hash maps.
   */
  public static <K, V> Shrink<IdentityHashMap<K, V>> shrinkIdentityHashMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, IdentityHashMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public IdentityHashMap<K, V> f(final Hashtable<K, V> h) {
        return new IdentityHashMap<K, V>(h);
      }
    }, new F<IdentityHashMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final IdentityHashMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  /**
   * A shrink strategy for linked hash maps.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for linked hash maps.
   */
  public static <K, V> Shrink<LinkedHashMap<K, V>> shrinkLinkedHashMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, LinkedHashMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public LinkedHashMap<K, V> f(final Hashtable<K, V> h) {
        return new LinkedHashMap<K, V>(h);
      }
    }, new F<LinkedHashMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final LinkedHashMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  /**
   * A shrink strategy for linked hash sets.
   * 
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for linked hash sets.
   */
  public static <A> Shrink<LinkedHashSet<A>> shrinkLinkedHashSet(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_LinkedHashSet(), Java.<A>LinkedHashSet_List());
  }

  /**
   * A shrink strategy for linked lists.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for linked lists.
   */
  public static <A> Shrink<LinkedList<A>> shrinkLinkedList(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_LinkedList(), Java.<A>LinkedList_List());
  }

  /**
   * A shrink strategy for priority queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for priority queues.
   */
  public static <A> Shrink<PriorityQueue<A>> shrinkPriorityQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_PriorityQueue(), Java.<A>PriorityQueue_List());
  }

  /**
   * A shrink strategy for properties.
   */
  public static final Shrink<Properties> shrinkProperties = shrinkHashtable(shrinkString, shrinkString)
      .map(new F<Hashtable<String, String>, Properties>() {
        @SuppressWarnings({"UseOfObsoleteCollectionType"})
        public Properties f(final Hashtable<String, String> h) {
          final Properties p = new Properties();

          for(final String k : h.keySet()) {
            p.setProperty(k, h.get(k));
          }

          return p;
        }
      }, new F<Properties, Hashtable<String, String>>() {
        @SuppressWarnings({"UseOfObsoleteCollectionType"})
        public Hashtable<String, String> f(final Properties p) {
          final Hashtable<String, String> t = new Hashtable<String, String>();

          for(final Object s : p.keySet()) {
            t.put((String)s, p.getProperty((String)s));
          }

          return t;
        }
      });

  /**
   * A shrink strategy for stacks.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for stacks.
   */
  public static <A> Shrink<Stack<A>> shrinkStack(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_Stack(), Java.<A>Stack_List());
  }

  /**
   * A shrink strategy for tree maps.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for tree maps.
   */
  public static <K, V> Shrink<TreeMap<K, V>> shrinkTreeMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, TreeMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public TreeMap<K, V> f(final Hashtable<K, V> h) {
        return new TreeMap<K, V>(h);
      }
    }, new F<TreeMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final TreeMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  /**
   * A shrink strategy for tree sets.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for tree sets.
   */
  public static <A> Shrink<TreeSet<A>> shrinkTreeSet(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_TreeSet(), Java.<A>TreeSet_List());
  }

  /**
   * A shrink strategy for vectors.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for vectors.
   */
  public static <A> Shrink<Vector<A>> shrinkVector(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_Vector(), Java.<A>Vector_List());
  }

  /**
   * A shrink strategy for weak hash maps.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for weak hash maps.
   */
  public static <K, V> Shrink<WeakHashMap<K, V>> shrinkWeakHashMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, WeakHashMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public WeakHashMap<K, V> f(final Hashtable<K, V> h) {
        return new WeakHashMap<K, V>(h);
      }
    }, new F<WeakHashMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final WeakHashMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  // END java.util

  // BEGIN java.util.concurrent

  /**
   * A shrink strategy for array blocking queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for array blocking queues.
   */
  public static <A> Shrink<ArrayBlockingQueue<A>> shrinkArrayBlockingQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_ArrayBlockingQueue(false), Java.<A>ArrayBlockingQueue_List());
  }

  /**
   * A shrink strategy for concurrent hash maps.
   *
   * @param sk The shrink strategy for keys.
   * @param sv The shrink stratgey for values.
   * @return A shrink strategy for concurrent hash maps.
   */
  public static <K, V> Shrink<ConcurrentHashMap<K, V>> shrinkConcurrentHashMap(final Shrink<K> sk, final Shrink<V> sv) {
    return shrinkHashtable(sk, sv).map(new F<Hashtable<K, V>, ConcurrentHashMap<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public ConcurrentHashMap<K, V> f(final Hashtable<K, V> h) {
        return new ConcurrentHashMap<K, V>(h);
      }
    }, new F<ConcurrentHashMap<K, V>, Hashtable<K, V>>() {
      @SuppressWarnings({"UseOfObsoleteCollectionType"})
      public Hashtable<K, V> f(final ConcurrentHashMap<K, V> m) {
        return new Hashtable<K, V>(m);
      }
    });
  }

  /**
   * A shrink strategy for concurrent linked queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for concurrent linked queues.
   */
  public static <A> Shrink<ConcurrentLinkedQueue<A>> shrinkConcurrentLinkedQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_ConcurrentLinkedQueue(), Java.<A>ConcurrentLinkedQueue_List());
  }

  /**
   * A shrink strategy for copy on write array lists.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for copy on write array lists.
   */
  public static <A> Shrink<CopyOnWriteArrayList<A>> shrinkCopyOnWriteArrayList(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_CopyOnWriteArrayList(), Java.<A>CopyOnWriteArrayList_List());
  }

  /**
   * A shrink strategy for copy on write array sets.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for copy on write array sets.
   */
  public static <A> Shrink<CopyOnWriteArraySet<A>> shrinkCopyOnWriteArraySet(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_CopyOnWriteArraySet(), Java.<A>CopyOnWriteArraySet_List());
  }

  /**
   * A shrink strategy for delay queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for delay queues.
   */
  public static <A extends Delayed> Shrink<DelayQueue<A>> shrinkDelayQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_DelayQueue(), Java.<A>DelayQueue_List());
  }

  /**
   * A shrink strategy for linked blocking queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for linked blocking queues.
   */
  public static <A> Shrink<LinkedBlockingQueue<A>> shrinkLinkedBlockingQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_LinkedBlockingQueue(), Java.<A>LinkedBlockingQueue_List());
  }

  /**
   * A shrink strategy for priority blocking queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for priority blocking queues.
   */
  public static <A> Shrink<PriorityBlockingQueue<A>> shrinkPriorityBlockingQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_PriorityBlockingQueue(), Java.<A>PriorityBlockingQueue_List());
  }

  /**
   * A shrink strategy for synchronous queues.
   *
   * @param sa The shrink strategy for the elements.
   * @return A shrink strategy for synchronous queues.
   */
  public static <A> Shrink<SynchronousQueue<A>> shrinkSynchronousQueue(final Shrink<A> sa) {
    return shrinkList(sa).map(Java.<A>List_SynchronousQueue(false), Java.<A>SynchronousQueue_List());
  }

  // END java.util.concurrent

  // BEGIN java.sql

  /**
   * A shrink strategy for SQL dates.
   */
  public static final Shrink<java.sql.Date> shrinkSQLDate =
      shrinkLong.map(new F<Long, java.sql.Date>() {
        public java.sql.Date f(final Long i) {
          return new java.sql.Date(i);
        }
      }, new F<java.sql.Date, Long>() {
        public Long f(final java.sql.Date c) {
          return c.getTime();
        }
      });

  /**
   * A shrink strategy for SQL times.
   */
  public static final Shrink<Time> shrinkTime =
      shrinkLong.map(new F<Long, Time>() {
        public Time f(final Long i) {
          return new Time(i);
        }
      }, new F<Time, Long>() {
        public Long f(final Time c) {
          return c.getTime();
        }
      });

  /**
   * A shrink strategy for SQL timestamps.
   */
  public static final Shrink<Timestamp> shrinkTimestamp =
      shrinkLong.map(new F<Long, Timestamp>() {
        public Timestamp f(final Long i) {
          return new Timestamp(i);
        }
      }, new F<Timestamp, Long>() {
        public Long f(final Timestamp c) {
          return c.getTime();
        }
      });

  // END java.sql

  // BEGIN java.math

  /**
   * A shrink strategy for big integers.
   */
  public static final Shrink<BigInteger> shrinkBigInteger = shrinkP2(shrinkByte, shrinkArray(shrinkByte)).map(new F<P2<Byte, Array<Byte>>, BigInteger>() {
      public BigInteger f(final P2<Byte, Array<Byte>> bs) {
        final byte[] x = new byte[bs._2().length() + 1];

        for(int i = 0; i < bs._2().array().length; i++) {
          x[i] = bs._2().array()[i];
        }

        x[bs._2().length()] = bs._1();

        return new BigInteger(x);
      }
    }, new F<BigInteger, P2<Byte, Array<Byte>>>() {
      public P2<Byte, Array<Byte>> f(final BigInteger i) {
        final byte[] b = i.toByteArray();
        final Byte[] x = new Byte[b.length - 1];
        arraycopy(b, 0, x, 0, b.length - 1);
        return p(b[0], array(x));
      }
    });

  /**
   * A shrink strategy for big decimals.
   */
  public static final Shrink<BigDecimal> shrinkBigDecimal =
      shrinkBigInteger.map(new F<BigInteger, BigDecimal>() {
        public BigDecimal f(final BigInteger i) {
          return new BigDecimal(i);
        }
      }, new F<BigDecimal, BigInteger>() {
        public BigInteger f(final BigDecimal d) {
          return d.toBigInteger();
        }
      });
  
  // END java.math

  /**
   * Returns a shrinking strategy for product-1 values.
   *
   * @param sa The shrinking strategy for the values.
   * @return a shrinking strategy for product-1 values.
   */
  public static <A> Shrink<P1<A>> shrinkP1(final Shrink<A> sa) {
    return new Shrink<P1<A>>(new F<P1<A>, Stream<P1<A>>>() {
      public Stream<P1<A>> f(final P1<A> p) {
        return sa.shrink(p._1()).map(new F<A, P1<A>>() {
          public P1<A> f(final A a) {
            return p(a);
          }
        });
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-2 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @return a shrinking strategy for product-2 values.
   */
  public static <A, B> Shrink<P2<A, B>> shrinkP2(final Shrink<A> sa, final Shrink<B> sb) {
    return new Shrink<P2<A, B>>(new F<P2<A, B>, Stream<P2<A, B>>>() {
      public Stream<P2<A, B>> f(final P2<A, B> p) {
        final F<A, F<B, P2<A, B>>> p2 = p2();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), p2);
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-3 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @param sc The shrinking strategy for the values.
   * @return a shrinking strategy for product-3 values.
   */
  public static <A, B, C> Shrink<P3<A, B, C>> shrinkP3(final Shrink<A> sa, final Shrink<B> sb, final Shrink<C> sc) {
    return new Shrink<P3<A,B,C>>(new F<P3<A, B, C>, Stream<P3<A, B, C>>>() {
      public Stream<P3<A, B, C>> f(final P3<A, B, C> p) {
        final F<A, F<B, F<C, P3<A, B, C>>>> p3 = p3();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), sc.shrink(p._3()), p3);
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-4 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @param sc The shrinking strategy for the values.
   * @param sd The shrinking strategy for the values.
   * @return a shrinking strategy for product-4 values.
   */
  public static <A, B, C, D> Shrink<P4<A, B, C, D>> shrinkP4(final Shrink<A> sa, final Shrink<B> sb, final Shrink<C> sc, final Shrink<D> sd) {
    return new Shrink<P4<A,B,C,D>>(new F<P4<A, B, C, D>, Stream<P4<A, B, C, D>>>() {
      public Stream<P4<A, B, C, D>> f(final P4<A, B, C, D> p) {
        final F<A, F<B, F<C, F<D, P4<A, B, C, D>>>>> p4 = p4();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), sc.shrink(p._3()), sd.shrink(p._4()), p4);
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-5 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @param sc The shrinking strategy for the values.
   * @param sd The shrinking strategy for the values.
   * @param se The shrinking strategy for the values.
   * @return a shrinking strategy for product-5 values.
   */
  public static <A, B, C, D, E> Shrink<P5<A, B, C, D, E>> shrinkP5(final Shrink<A> sa, final Shrink<B> sb, final Shrink<C> sc, final Shrink<D> sd, final Shrink<E> se) {
    return new Shrink<P5<A,B,C,D,E>>(new F<P5<A, B, C, D, E>, Stream<P5<A, B, C, D, E>>>() {
      public Stream<P5<A, B, C, D, E>> f(final P5<A, B, C, D, E> p) {
        final F<A, F<B, F<C, F<D, F<E, P5<A, B, C, D, E>>>>>> p5 = p5();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), sc.shrink(p._3()), sd.shrink(p._4()), se.shrink(p._5()), p5);
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-6 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @param sc The shrinking strategy for the values.
   * @param sd The shrinking strategy for the values.
   * @param se The shrinking strategy for the values.
   * @param sf The shrinking strategy for the values.
   * @return a shrinking strategy for product-6 values.
   */
  public static <A, B, C, D, E, F$> Shrink<P6<A, B, C, D, E, F$>> shrinkP6(final Shrink<A> sa, final Shrink<B> sb, final Shrink<C> sc, final Shrink<D> sd, final Shrink<E> se, final Shrink<F$> sf) {
    return new Shrink<P6<A,B,C,D,E,F$>>(new F<P6<A, B, C, D, E, F$>, Stream<P6<A, B, C, D, E, F$>>>() {
      public Stream<P6<A, B, C, D, E, F$>> f(final P6<A, B, C, D, E, F$> p) {
        final F<A, F<B, F<C, F<D, F<E, F<F$, P6<A, B, C, D, E, F$>>>>>>> p6 = p6();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), sc.shrink(p._3()), sd.shrink(p._4()), se.shrink(p._5()), sf.shrink(p._6()), p6);
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-7 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @param sc The shrinking strategy for the values.
   * @param sd The shrinking strategy for the values.
   * @param se The shrinking strategy for the values.
   * @param sf The shrinking strategy for the values.
   * @param sg The shrinking strategy for the values.
   * @return a shrinking strategy for product-7 values.
   */
  public static <A, B, C, D, E, F$, G> Shrink<P7<A, B, C, D, E, F$, G>> shrinkP7(final Shrink<A> sa, final Shrink<B> sb, final Shrink<C> sc, final Shrink<D> sd, final Shrink<E> se, final Shrink<F$> sf, final Shrink<G> sg) {
    return new Shrink<P7<A,B,C,D,E,F$,G>>(new F<P7<A, B, C, D, E, F$, G>, Stream<P7<A, B, C, D, E, F$, G>>>() {
      public Stream<P7<A, B, C, D, E, F$, G>> f(final P7<A, B, C, D, E, F$, G> p) {
        final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, P7<A, B, C, D, E, F$, G>>>>>>>> p7 = p7();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), sc.shrink(p._3()), sd.shrink(p._4()), se.shrink(p._5()), sf.shrink(p._6()), sg.shrink(p._7()), p7);
      }
    });
  }

  /**
   * Returns a shrinking strategy for product-8 values.
   *
   * @param sa The shrinking strategy for the values.
   * @param sb The shrinking strategy for the values.
   * @param sc The shrinking strategy for the values.
   * @param sd The shrinking strategy for the values.
   * @param se The shrinking strategy for the values.
   * @param sf The shrinking strategy for the values.
   * @param sg The shrinking strategy for the values.
   * @param sh The shrinking strategy for the values.
   * @return a shrinking strategy for product-8 values.
   */
  public static <A, B, C, D, E, F$, G, H> Shrink<P8<A, B, C, D, E, F$, G, H>> shrinkP8(final Shrink<A> sa, final Shrink<B> sb, final Shrink<C> sc, final Shrink<D> sd, final Shrink<E> se, final Shrink<F$> sf, final Shrink<G> sg, final Shrink<H> sh) {
    return new Shrink<P8<A,B,C,D,E,F$,G,H>>(new F<P8<A, B, C, D, E, F$, G, H>, Stream<P8<A, B, C, D, E, F$, G, H>>>() {
      public Stream<P8<A, B, C, D, E, F$, G, H>> f(final P8<A, B, C, D, E, F$, G, H> p) {
        final F<A, F<B, F<C, F<D, F<E, F<F$, F<G, F<H, P8<A, B, C, D, E, F$, G, H>>>>>>>>> p8 = p8();
        return sa.shrink(p._1()).bind(sb.shrink(p._2()), sc.shrink(p._3()), sd.shrink(p._4()), se.shrink(p._5()), sf.shrink(p._6()), sg.shrink(p._7()), sh.shrink(p._8()), p8);
      }
    });
  }
}
