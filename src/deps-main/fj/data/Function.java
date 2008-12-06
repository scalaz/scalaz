package fj.data;

import fj.Effect;
import fj.F;
import fj.P1;
import static fj.data.List.asString;
import static fj.data.List.fromString;

/**
 * Functions that convert between data structure types.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Function {
  private Function() {
    throw new UnsupportedOperationException();
  }

  // BEGIN List ->

  /**
   * A function that converts lists to arrays.
   *
   * @return A function that converts lists to arrays.
   */
  public static <A> F<List<A>, Array<A>> List_Array() {
    return new F<List<A>, Array<A>>() {
      public Array<A> f(final List<A> as) {
        return as.toArray();
      }
    };
  }

  /**
   * A function that converts lists to streams.
   *
   * @return A function that converts lists to streams.
   */
  public static <A> F<List<A>, Stream<A>> List_Stream() {
    return new F<List<A>, Stream<A>>() {
      public Stream<A> f(final List<A> as) {
        return as.toStream();
      }
    };
  }

  /**
   * A function that converts lists to options.
   *
   * @return A function that converts lists to options.
   */
  public static <A> F<List<A>, Option<A>> List_Option() {
    return new F<List<A>, Option<A>>() {
      public Option<A> f(final List<A> as) {
        return as.toOption();
      }
    };
  }

  /**
   * A function that converts lists to eithers.
   *
   * @return A function that converts lists to eithers.
   */
  public static <A, B> F<P1<A>, F<List<B>, Either<A, B>>> List_Either() {
    return new F<P1<A>, F<List<B>, Either<A, B>>>() {
      public F<List<B>, Either<A, B>> f(final P1<A> a) {
        return new F<List<B>, Either<A, B>>() {
          public Either<A, B> f(final List<B> bs) {
            return bs.toEither(a);
          }
        };
      }
    };
  }

  /**
   * A function that converts lists to strings.
   */
  public static final F<List<Character>, String> List_String = new F<List<Character>, String>() {
    public String f(final List<Character> cs) {
      return asString(cs);
    }
  };

  /**
   * A function that converts lists to string buffers.
   */
  public static final F<List<Character>, StringBuffer> List_StringBuffer = new F<List<Character>, StringBuffer>() {
    public StringBuffer f(final List<Character> cs) {
      return new StringBuffer(asString(cs));
    }
  };

  /**
   * A function that converts lists to string builders.
   */
  public static final F<List<Character>, StringBuilder> List_StringBuilder = new F<List<Character>, StringBuilder>() {
    public StringBuilder f(final List<Character> cs) {
      return new StringBuilder(asString(cs));
    }
  };

  // END List ->

  // BEGIN Array ->

  /**
   * A function that converts arrays to lists.
   *
   * @return A function that converts arrays to lists.
   */
  public static <A> F<Array<A>, List<A>> Array_List() {
    return new F<Array<A>, List<A>>() {
      public List<A> f(final Array<A> as) {
        return as.toList();
      }
    };
  }

  /**
   * A function that converts arrays to streams.
   *
   * @return A function that converts arrays to streams.
   */
  public static <A> F<Array<A>, Stream<A>> Array_Stream() {
    return new F<Array<A>, Stream<A>>() {
      public Stream<A> f(final Array<A> as) {
        return as.toStream();
      }
    };
  }

  /**
   * A function that converts arrays to options.
   *
   * @return A function that converts arrays to options.
   */
  public static <A> F<Array<A>, Option<A>> Array_Option() {
    return new F<Array<A>, Option<A>>() {
      public Option<A> f(final Array<A> as) {
        return as.toOption();
      }
    };
  }
  /**
   * A function that converts arrays to eithers.
   *
   * @return A function that converts arrays to eithers.
   */
  public static <A, B> F<P1<A>, F<Array<B>, Either<A, B>>> Array_Either() {
    return new F<P1<A>, F<Array<B>, Either<A, B>>>() {
      public F<Array<B>, Either<A, B>> f(final P1<A> a) {
        return new F<Array<B>, Either<A, B>>() {
          public Either<A, B> f(final Array<B> bs) {
            return bs.toEither(a);
          }
        };
      }
    };
  }

  /**
   * A function that converts arrays to strings.
   */
  public static final F<Array<Character>, String> Array_String = new F<Array<Character>, String>() {
    public String f(final Array<Character> cs) {
      final StringBuilder sb = new StringBuilder();
      cs.foreach(new Effect<Character>() {
        public void e(final Character c) {
          sb.append(c);
        }
      });
      return sb.toString();
    }
  };

  /**
   * A function that converts arrays to string buffers.
   */
  public static final F<Array<Character>, StringBuffer> Array_StringBuffer = new F<Array<Character>, StringBuffer>() {
    public StringBuffer f(final Array<Character> cs) {
      final StringBuffer sb = new StringBuffer();
      cs.foreach(new Effect<Character>() {
        public void e(final Character c) {
          sb.append(c);
        }
      });
      return sb;
    }
  };

  /**
   * A function that converts arrays to string builders.
   */
  public static final F<Array<Character>, StringBuilder> Array_StringBuilder = new F<Array<Character>, StringBuilder>() {
    public StringBuilder f(final Array<Character> cs) {
      final StringBuilder sb = new StringBuilder();
      cs.foreach(new Effect<Character>() {
        public void e(final Character c) {
          sb.append(c);
        }
      });
      return sb;
    }
  };

  // END Array ->

  // BEGIN Stream ->

  /**
   * A function that converts streams to lists.
   *
   * @return A function that converts streams to lists.
   */
  public static <A> F<Stream<A>, List<A>> Stream_List() {
    return new F<Stream<A>, List<A>>() {
      public List<A> f(final Stream<A> as) {
        return as.toList();
      }
    };
  }

  /**
   * A function that converts streams to arrays.
   *
   * @return A function that converts streams to arrays.
   */
  public static <A> F<Stream<A>, Array<A>> Stream_Array() {
    return new F<Stream<A>, Array<A>>() {
      public Array<A> f(final Stream<A> as) {
        return as.toArray();
      }
    };
  }

  /**
   * A function that converts streams to options.
   *
   * @return A function that converts streams to options.
   */
  public static <A> F<Stream<A>, Option<A>> Stream_Option() {
    return new F<Stream<A>, Option<A>>() {
      public Option<A> f(final Stream<A> as) {
        return as.toOption();
      }
    };
  }

  /**
   * A function that converts streams to eithers.
   *
   * @return A function that converts streams to eithers.
   */
  public static <A, B> F<P1<A>, F<Stream<B>, Either<A, B>>> Stream_Either() {
    return new F<P1<A>, F<Stream<B>, Either<A, B>>>() {
      public F<Stream<B>, Either<A, B>> f(final P1<A> a) {
        return new F<Stream<B>, Either<A, B>>() {
          public Either<A, B> f(final Stream<B> bs) {
            return bs.toEither(a);
          }
        };
      }
    };
  }

  /**
   * A function that converts streams to strings.
   */
  public static final F<Stream<Character>, String> Stream_String = new F<Stream<Character>, String>() {
    public String f(final Stream<Character> cs) {
      final StringBuilder sb = new StringBuilder();
      cs.foreach(new Effect<Character>() {
        public void e(final Character c) {
          sb.append(c);
        }
      });
      return sb.toString();
    }
  };

  /**
   * A function that converts streams to string buffers.
   */
  public static final F<Stream<Character>, StringBuffer> Stream_StringBuffer = new F<Stream<Character>, StringBuffer>() {
    public StringBuffer f(final Stream<Character> cs) {
      final StringBuffer sb = new StringBuffer();
      cs.foreach(new Effect<Character>() {
        public void e(final Character c) {
          sb.append(c);
        }
      });
      return sb;
    }
  };

  /**
   * A function that converts streams to string builders.
   */
  public static final F<Stream<Character>, StringBuilder> Stream_StringBuilder = new F<Stream<Character>, StringBuilder>() {
    public StringBuilder f(final Stream<Character> cs) {
      final StringBuilder sb = new StringBuilder();
      cs.foreach(new Effect<Character>() {
        public void e(final Character c) {
          sb.append(c);
        }
      });
      return sb;
    }
  };

  // END Stream ->

  // BEGIN Option ->

  /**
   * A function that converts options to lists.
   *
   * @return A function that converts options to lists.
   */
  public static <A> F<Option<A>, List<A>> Option_List() {
    return new F<Option<A>, List<A>>() {
      public List<A> f(final Option<A> o) {
        return o.toList();
      }
    };
  }

  /**
   * A function that converts options to arrays.
   *
   * @return A function that converts options to arrays.
   */
  public static <A> F<Option<A>, Array<A>> Option_Array() {
    return new F<Option<A>, Array<A>>() {
      public Array<A> f(final Option<A> o) {
        return o.toArray();
      }
    };
  }

  /**
   * A function that converts options to streams.
   *
   * @return A function that converts options to streams.
   */
  public static <A> F<Option<A>, Stream<A>> Option_Stream() {
    return new F<Option<A>, Stream<A>>() {
      public Stream<A> f(final Option<A> o) {
        return o.toStream();
      }
    };
  }

  /**
   * A function that converts options to eithers.
   *
   * @return A function that converts options to eithers.
   */
  public static <A, B> F<P1<A>, F<Option<B>, Either<A, B>>> Option_Either() {
    return new F<P1<A>, F<Option<B>, Either<A, B>>>() {
      public F<Option<B>, Either<A, B>> f(final P1<A> a) {
        return new F<Option<B>, Either<A, B>>() {
          public Either<A, B> f(final Option<B> o) {
            return o.toEither(a);
          }
        };
      }
    };
  }
  
  /**
   * A function that converts options to strings.
   */
  public static final F<Option<Character>, String> Option_String = new F<Option<Character>, String>() {
    public String f(final Option<Character> o) {
      return asString(o.toList());
    }
  };

  /**
   * A function that converts options to string buffers.
   */
  public static final F<Option<Character>, StringBuffer> Option_StringBuffer = new F<Option<Character>, StringBuffer>() {
    public StringBuffer f(final Option<Character> o) {
      return new StringBuffer(asString(o.toList()));
    }
  };

  /**
   * A function that converts options to string builders.
   */
  public static final F<Option<Character>, StringBuilder> Option_StringBuilder = new F<Option<Character>, StringBuilder>() {
    public StringBuilder f(final Option<Character> o) {
      return new StringBuilder(asString(o.toList()));
    }
  };

  // END Option ->

  // BEGIN Either ->

  /**
   * A function that converts eithers to lists.
   *
   * @return A function that converts eithers to lists.
   */
  public static <A, B> F<Either<A, B>, List<A>> Either_ListA() {
    return new F<Either<A, B>, List<A>>() {
      public List<A> f(final Either<A, B> e) {
        return e.left().toList();
      }
    };
  }

  /**
   * A function that converts eithers to lists.
   *
   * @return A function that converts eithers to lists.
   */
  public static <A, B> F<Either<A, B>, List<B>> Either_ListB() {
    return new F<Either<A, B>, List<B>>() {
      public List<B> f(final Either<A, B> e) {
        return e.right().toList();
      }
    };
  }

  /**
   * A function that converts eithers to arrays.
   *
   * @return A function that converts eithers to arrays.
   */
  public static <A, B> F<Either<A, B>, Array<A>> Either_ArrayA() {
    return new F<Either<A, B>, Array<A>>() {
      public Array<A> f(final Either<A, B> e) {
        return e.left().toArray();
      }
    };
  }

  /**
   * A function that converts eithers to arrays.
   *
   * @return A function that converts eithers to arrays.
   */
  public static <A, B> F<Either<A, B>, Array<B>> Either_ArrayB() {
    return new F<Either<A, B>, Array<B>>() {
      public Array<B> f(final Either<A, B> e) {
        return e.right().toArray();
      }
    };
  }

  /**
   * A function that converts eithers to streams.
   *
   * @return A function that converts eithers to streams.
   */
  public static <A, B> F<Either<A, B>, Stream<A>> Either_StreamA() {
    return new F<Either<A, B>, Stream<A>>() {
      public Stream<A> f(final Either<A, B> e) {
        return e.left().toStream();
      }
    };
  }

  /**
   * A function that converts eithers to streams.
   *
   * @return A function that converts eithers to streams.
   */
  public static <A, B> F<Either<A, B>, Stream<B>> Either_StreamB() {
    return new F<Either<A, B>, Stream<B>>() {
      public Stream<B> f(final Either<A, B> e) {
        return e.right().toStream();
      }
    };
  }

  /**
   * A function that converts eithers to options.
   *
   * @return A function that converts eithers to options.
   */
  public static <A, B> F<Either<A, B>, Option<A>> Either_OptionA() {
    return new F<Either<A, B>, Option<A>>() {
      public Option<A> f(final Either<A, B> e) {
        return e.left().toOption();
      }
    };
  }

  /**
   * A function that converts eithers to options.
   *
   * @return A function that converts eithers to options.
   */
  public static <A, B> F<Either<A, B>, Option<B>> Either_OptionB() {
    return new F<Either<A, B>, Option<B>>() {
      public Option<B> f(final Either<A, B> e) {
        return e.right().toOption();
      }
    };
  }

  /**
   * A function that converts eithers to strings.
   *
   * @return A function that converts eithers to strings.
   */
  public static <B> F<Either<Character, B>, String> Either_StringA() {
    return new F<Either<Character, B>, String>() {
      public String f(final Either<Character, B> e) {
        return asString(e.left().toList());
      }
    };
  }

  /**
   * A function that converts eithers to strings.
   *
   * @return A function that converts eithers to strings.
   */
  public static <A> F<Either<A, Character>, String> Either_StringB() {
    return new F<Either<A, Character>, String>() {
      public String f(final Either<A, Character> e) {
        return asString(e.right().toList());
      }
    };
  }

  /**
   * A function that converts eithers to string buffers.
   *
   * @return A function that converts eithers to string buffers.
   */
  public static <B> F<Either<Character, B>, StringBuffer> Either_StringBufferA() {
    return new F<Either<Character, B>, StringBuffer>() {
      public StringBuffer f(final Either<Character, B> e) {
        return new StringBuffer(asString(e.left().toList()));
      }
    };
  }

  /**
   * A function that converts eithers to string buffers.
   *
   * @return A function that converts eithers to string buffers.
   */
  public static <A> F<Either<A, Character>, StringBuffer> Either_StringBufferB() {
    return new F<Either<A, Character>, StringBuffer>() {
      public StringBuffer f(final Either<A, Character> e) {
        return new StringBuffer(asString(e.right().toList()));
      }
    };
  }

  /**
   * A function that converts eithers to string builders.
   *
   * @return A function that converts eithers to string builders.
   */
  public static <B> F<Either<Character, B>, StringBuilder> Either_StringBuilderA() {
    return new F<Either<Character, B>, StringBuilder>() {
      public StringBuilder f(final Either<Character, B> e) {
        return new StringBuilder(asString(e.left().toList()));
      }
    };
  }

  /**
   * A function that converts eithers to string builders.
   *
   * @return A function that converts eithers to string builders.
   */
  public static <A> F<Either<A, Character>, StringBuilder> Either_StringBuilderB() {
    return new F<Either<A, Character>, StringBuilder>() {
      public StringBuilder f(final Either<A, Character> e) {
        return new StringBuilder(asString(e.right().toList()));
      }
    };
  }

  // END Either ->
   
  // BEGIN String ->

  /**
   * A function that converts strings to lists.
   */
  public static final F<String, List<Character>> String_List = new F<String, List<Character>>() {
    public List<Character> f(final String s) {
      return fromString(s);
    }
  };

  /**
   * A function that converts strings to arrays.
   */
  public static final F<String, Array<Character>> String_Array = new F<String, Array<Character>>() {
    public Array<Character> f(final String s) {
      return fromString(s).toArray();
    }
  };

  /**
   * A function that converts strings to options.
   */
  public static final F<String, Option<Character>> String_Option = new F<String, Option<Character>>() {
    public Option<Character> f(final String s) {
      return fromString(s).toOption();
    }
  };

  /**
   * A function that converts string to eithers.
   *
   * @return A function that converts string to eithers.
   */
  public static <A> F<P1<A>, F<String, Either<A, Character>>> String_Either() {
    return new F<P1<A>, F<String, Either<A, Character>>>() {
      public F<String, Either<A, Character>> f(final P1<A> a) {
        return new F<String, Either<A, Character>>() {
          public Either<A, Character> f(final String s) {
            return fromString(s).toEither(a);
          }
        };
      }
    };
  }

  /**
   * A function that converts strings to streams.
   */
  public static final F<String, Stream<Character>> String_Stream = new F<String, Stream<Character>>() {
    public Stream<Character> f(final String s) {
      return fromString(s).toStream();
    }
  };

  /**
   * A function that converts strings to string buffers.
   */
  public static final F<String, StringBuffer> String_StringBuffer = new F<String, StringBuffer>() {
    public StringBuffer f(final String s) {
      return new StringBuffer(s);
    }
  };

  /**
   * A function that converts strings to string builders.
   */
  public static final F<String, StringBuilder> String_StringBuilder = new F<String, StringBuilder>() {
    public StringBuilder f(final String s) {
      return new StringBuilder(s);
    }
  };

  // END String ->

  // BEGIN StringBuffer ->

  /**
   * A function that converts string buffers to lists.
   */
  public static final F<StringBuffer, List<Character>> StringBuffer_List = new F<StringBuffer, List<Character>>() {
    public List<Character> f(final StringBuffer s) {
      return fromString(s.toString());
    }
  };

  /**
   * A function that converts string buffers to arrays.
   */
  public static final F<StringBuffer, Array<Character>> StringBuffer_Array = new F<StringBuffer, Array<Character>>() {
    public Array<Character> f(final StringBuffer s) {
      return fromString(s.toString()).toArray();
    }
  };

  /**
   * A function that converts string buffers to streams.
   */
  public static final F<StringBuffer, Stream<Character>> StringBuffer_Stream = new F<StringBuffer, Stream<Character>>() {
    public Stream<Character> f(final StringBuffer s) {
      return fromString(s.toString()).toStream();
    }
  };

  /**
   * A function that converts string buffers to options.
   */
  public static final F<StringBuffer, Option<Character>> StringBuffer_Option = new F<StringBuffer, Option<Character>>() {
    public Option<Character> f(final StringBuffer s) {
      return fromString(s.toString()).toOption();
    }
  };

  /**
   * A function that converts string buffers to eithers.
   *
   * @return A function that converts string buffers to eithers.
   */
  public static <A> F<P1<A>, F<StringBuffer, Either<A, Character>>> StringBuffer_Either() {
    return new F<P1<A>, F<StringBuffer, Either<A, Character>>>() {
      public F<StringBuffer, Either<A, Character>> f(final P1<A> a) {
        return new F<StringBuffer, Either<A, Character>>() {
          public Either<A, Character> f(final StringBuffer s) {
            return fromString(s.toString()).toEither(a);
          }
        };
      }
    };
  }

  /**
   * A function that converts string buffers to strings.
   */
  public static final F<StringBuffer, String> StringBuffer_String = new F<StringBuffer, String>() {
    public String f(final StringBuffer s) {
      return s.toString();
    }
  };

  /**
   * A function that converts string buffers to string builders.
   */
  public static final F<StringBuffer, StringBuilder> StringBuffer_StringBuilder = new F<StringBuffer, StringBuilder>() {
    public StringBuilder f(final StringBuffer s) {
      return new StringBuilder(s);
    }
  };

  // END StringBuffer ->

  // BEGIN StringBuilder ->

  /**
   * A function that converts string builders to lists.
   */
  public static final F<StringBuilder, List<Character>> StringBuilder_List = new F<StringBuilder, List<Character>>() {
    public List<Character> f(final StringBuilder s) {
      return fromString(s.toString());
    }
  };

  /**
   * A function that converts string builders to arrays.
   */
  public static final F<StringBuilder, Array<Character>> StringBuilder_Array = new F<StringBuilder, Array<Character>>() {
    public Array<Character> f(final StringBuilder s) {
      return fromString(s.toString()).toArray();
    }
  };

  /**
   * A function that converts string builders to streams.
   */
  public static final F<StringBuilder, Stream<Character>> StringBuilder_Stream = new F<StringBuilder, Stream<Character>>() {
    public Stream<Character> f(final StringBuilder s) {
      return fromString(s.toString()).toStream();
    }
  };

  /**
   * A function that converts string builders to options.
   */
  public static final F<StringBuilder, Option<Character>> StringBuilder_Option = new F<StringBuilder, Option<Character>>() {
    public Option<Character> f(final StringBuilder s) {
      return fromString(s.toString()).toOption();
    }
  };

  /**
   * A function that converts string builders to eithers.
   *
   * @return A function that converts string builders to eithers.
   */
  public static <A> F<P1<A>, F<StringBuilder, Either<A, Character>>> StringBuilder_Either() {
    return new F<P1<A>, F<StringBuilder, Either<A, Character>>>() {
      public F<StringBuilder, Either<A, Character>> f(final P1<A> a) {
        return new F<StringBuilder, Either<A, Character>>() {
          public Either<A, Character> f(final StringBuilder s) {
            return fromString(s.toString()).toEither(a);
          }
        };
      }
    };
  }

  /**
   * A function that converts string builders to strings.
   */
  public static final F<StringBuilder, String> StringBuilder_String = new F<StringBuilder, String>() {
    public String f(final StringBuilder s) {
      return s.toString();
    }
  };

  /**
   * A function that converts string builders to string buffers.
   */
  public static final F<StringBuilder, StringBuffer> StringBuilder_StringBuffer = new F<StringBuilder, StringBuffer>() {
    public StringBuffer f(final StringBuilder s) {
      return new StringBuffer(s);
    }
  };
  
  // END StringBuilder ->
}
