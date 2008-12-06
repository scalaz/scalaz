package fj;

/**
 * Functions that convert between Java primitive types.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Primitive {
  private Primitive() {
    throw new UnsupportedOperationException();
  }

  // BEGIN Boolean ->

  /**
   * A function that converts booleans to bytes.
   */
  public static final F<Boolean, Byte> Boolean_Byte = new F<Boolean, Byte>() {
    public Byte f(final Boolean b) {
      return (byte)(b ? 1 : 0);
    }
  };

  /**
   * A function that converts booleans to characters.
   */
  public static final F<Boolean, Character> Boolean_Character = new F<Boolean, Character>() {
    public Character f(final Boolean b) {
      return (char)(b ? 1 : 0);
    }
  };

  /**
   * A function that converts booleans to doubles.
   */
  public static final F<Boolean, Double> Boolean_Double = new F<Boolean, Double>() {
    public Double f(final Boolean b) {
      return b ? 1D : 0D;
    }
  };

  /**
   * A function that converts booleans to floats.
   */
  public static final F<Boolean, Float> Boolean_Float = new F<Boolean, Float>() {
    public Float f(final Boolean b) {
      return b ? 1F : 0F;
    }
  };

  /**
   * A function that converts booleans to integers.
   */
  public static final F<Boolean, Integer> Boolean_Integer = new F<Boolean, Integer>() {
    public Integer f(final Boolean b) {
      return b ? 1 : 0;
    }
  };

  /**
   * A function that converts booleans to longs.
   */
  public static final F<Boolean, Long> Boolean_Long = new F<Boolean, Long>() {
    public Long f(final Boolean b) {
      return b ? 1L : 0L;
    }
  };

  /**
   * A function that converts booleans to shorts.
   */
  public static final F<Boolean, Short> Boolean_Short = new F<Boolean, Short>() {
    public Short f(final Boolean b) {
      return (short)(b ? 1 : 0);
    }
  };

  // END Boolean ->

  // BEGIN Byte ->

  /**
   * A function that converts bytes to booleans.
   */
  public static final F<Byte, Boolean> Byte_Boolean = new F<Byte, Boolean>() {
    public Boolean f(final Byte b) {
      return b != 0;
    }
  };

  /**
   * A function that converts bytes to characters.
   */
  public static final F<Byte, Character> Byte_Character = new F<Byte, Character>() {
    public Character f(final Byte b) {
      return (char)(byte)b;
    }
  };

  /**
   * A function that converts bytes to doubles.
   */
  public static final F<Byte, Double> Byte_Double = new F<Byte, Double>() {
    public Double f(final Byte b) {
      //No it isn't
      //noinspection RedundantCast
      return (double)b;
    }
  };

  /**
   * A function that converts bytes to floats.
   */
  public static final F<Byte, Float> Byte_Float = new F<Byte, Float>() {
    public Float f(final Byte b) {
      //No it isn't
      //noinspection RedundantCast
      return (float)b;
    }
  };

  /**
   * A function that converts bytes to integers.
   */
  public static final F<Byte, Integer> Byte_Integer = new F<Byte, Integer>() {
    public Integer f(final Byte b) {
      //No it isn't
      //noinspection RedundantCast
      return (int)b;
    }
  };

  /**
   * A function that converts bytes to longs.
   */
  public static final F<Byte, Long> Byte_Long = new F<Byte, Long>() {
    public Long f(final Byte b) {
      //No it isn't
      //noinspection RedundantCast
      return (long)b;
    }
  };

  /**
   * A function that converts bytes to shorts.
   */
  public static final F<Byte, Short> Byte_Short = new F<Byte, Short>() {
    public Short f(final Byte b) {
      //No it isn't
      //noinspection RedundantCast
      return (short)b;
    }
  };

  // END Byte ->

  // BEGIN Character ->

  /**
   * A function that converts characters to booleans.
   */
  public static final F<Character, Boolean> Character_Boolean = new F<Character, Boolean>() {
    public Boolean f(final Character c) {
      return c != 0;
    }
  };

  /**
   * A function that converts characters to bytes.
   */
  public static final F<Character, Byte> Character_Byte = new F<Character, Byte>() {
    public Byte f(final Character c) {
      return (byte)(char)c;
    }
  };

  /**
   * A function that converts characters to doubles.
   */
  public static final F<Character, Double> Character_Double = new F<Character, Double>() {
    public Double f(final Character c) {
      return (double)(char)c;
    }
  };

  /**
   * A function that converts characters to floats.
   */
  public static final F<Character, Float> Character_Float = new F<Character, Float>() {
    public Float f(final Character c) {
      return (float)(char)c;
    }
  };

  /**
   * A function that converts characters to integers.
   */
  public static final F<Character, Integer> Character_Integer = new F<Character, Integer>() {
    public Integer f(final Character c) {
      return (int)(char)c;
    }
  };

  /**
   * A function that converts characters to longs.
   */
  public static final F<Character, Long> Character_Long = new F<Character, Long>() {
    public Long f(final Character c) {
      return (long)(char)c;
    }
  };

  /**
   * A function that converts characters to shorts.
   */
  public static final F<Character, Short> Character_Short = new F<Character, Short>() {
    public Short f(final Character c) {
      return (short)(char)c;
    }
  };

  // END Character ->

  // BEGIN Double ->

  /**
   * A function that converts doubles to booleans.
   */
  public static final F<Double, Boolean> Double_Boolean = new F<Double, Boolean>() {
    public Boolean f(final Double d) {
      return d != 0D;
    }
  };

  /**
   * A function that converts doubles to bytes.
   */
  public static final F<Double, Byte> Double_Byte = new F<Double, Byte>() {
    public Byte f(final Double d) {
      return (byte)(double)d;
    }
  };

  /**
   * A function that converts doubles to characters.
   */
  public static final F<Double, Character> Double_Character = new F<Double, Character>() {
    public Character f(final Double d) {
      return (char)(double)d;
    }
  };

  /**
   * A function that converts doubles to floats.
   */
  public static final F<Double, Float> Double_Float = new F<Double, Float>() {
    public Float f(final Double d) {
      return (float)(double)d;
    }
  };

  /**
   * A function that converts doubles to integers.
   */
  public static final F<Double, Integer> Double_Integer = new F<Double, Integer>() {
    public Integer f(final Double d) {
      return (int)(double)d;
    }
  };

  /**
   * A function that converts doubles to longs.
   */
  public static final F<Double, Long> Double_Long = new F<Double, Long>() {
    public Long f(final Double d) {
      return (long)(double)d;
    }
  };

  /**
   * A function that converts doubles to shorts.
   */
  public static final F<Double, Short> Double_Short = new F<Double, Short>() {
    public Short f(final Double d) {
      return (short)(double)d;
    }
  };

  // END Double ->

  // BEGIN Float ->

  /**
   * A function that converts floats to booleans.
   */
  public static final F<Float, Boolean> Float_Boolean = new F<Float, Boolean>() {
    public Boolean f(final Float f) {
      return f != 0F;
    }
  };

  /**
   * A function that converts floats to bytes.
   */
  public static final F<Float, Byte> Float_Byte = new F<Float, Byte>() {
    public Byte f(final Float f) {
      return (byte)(float)f;
    }
  };

  /**
   * A function that converts floats to characters.
   */
  public static final F<Float, Character> Float_Character = new F<Float, Character>() {
    public Character f(final Float f) {
      return (char)(float)f;
    }
  };

  /**
   * A function that converts floats to doubles.
   */
  public static final F<Float, Double> Float_Double = new F<Float, Double>() {
    public Double f(final Float f) {
      return (double)(float)f;
    }
  };

  /**
   * A function that converts floats to integers.
   */
  public static final F<Float, Integer> Float_Integer = new F<Float, Integer>() {
    public Integer f(final Float f) {
      return (int)(float)f;
    }
  };

  /**
   * A function that converts floats to longs.
   */
  public static final F<Float, Long> Float_Long = new F<Float, Long>() {
    public Long f(final Float f) {
      return (long)(float)f;
    }
  };

  /**
   * A function that converts floats to shorts.
   */
  public static final F<Float, Short> Float_Short = new F<Float, Short>() {
    public Short f(final Float f) {
      return (short)(float)f;
    }
  };

  // END Float ->

  // BEGIN Integer ->

  /**
   * A function that converts integers to booleans.
   */
  public static final F<Integer, Boolean> Integer_Boolean = new F<Integer, Boolean>() {
    public Boolean f(final Integer i) {
      return i != 0;
    }
  };

  /**
   * A function that converts integers to bytes.
   */
  public static final F<Integer, Byte> Integer_Byte = new F<Integer, Byte>() {
    public Byte f(final Integer i) {
      return (byte)(int)i;
    }
  };

  /**
   * A function that converts integers to characters.
   */
  public static final F<Integer, Character> Integer_Character = new F<Integer, Character>() {
    public Character f(final Integer i) {
      return (char)(int)i;
    }
  };

  /**
   * A function that converts integers to doubles.
   */
  public static final F<Integer, Double> Integer_Double = new F<Integer, Double>() {
    public Double f(final Integer i) {
      //No it isn't
      //noinspection RedundantCast
      return (double)i;
    }
  };

  /**
   * A function that converts integers to floats.
   */
  public static final F<Integer, Float> Integer_Float = new F<Integer, Float>() {
    public Float f(final Integer i) {
      //No it isn't
      //noinspection RedundantCast
      return (float)i;
    }
  };

  /**
   * A function that converts integers to longs.
   */
  public static final F<Integer, Long> Integer_Long = new F<Integer, Long>() {
    public Long f(final Integer i) {
      //No it isn't
      //noinspection RedundantCast
      return (long)i;
    }
  };

  /**
   * A function that converts integers to shorts.
   */
  public static final F<Integer, Short> Integer_Short = new F<Integer, Short>() {
    public Short f(final Integer i) {
      //No it isn't
      //noinspection RedundantCast
      return (short)(int)i;
    }
  };

  // END Integer ->

  // BEGIN Long ->

  /**
   * A function that converts longs to booleans.
   */
  public static final F<Long, Boolean> Long_Boolean = new F<Long, Boolean>() {
    public Boolean f(final Long l) {
      return l != 0L;
    }
  };

  /**
   * A function that converts longs to bytes.
   */
  public static final F<Long, Byte> Long_Byte = new F<Long, Byte>() {
    public Byte f(final Long l) {
      return (byte)(long)l;
    }
  };

  /**
   * A function that converts longs to characters.
   */
  public static final F<Long, Character> Long_Character = new F<Long, Character>() {
    public Character f(final Long l) {
      return (char)(long)l;
    }
  };

  /**
   * A function that converts longs to doubles.
   */
  public static final F<Long, Double> Long_Double = new F<Long, Double>() {
    public Double f(final Long l) {
      return (double)(long)l;
    }
  };

  /**
   * A function that converts longs to floats.
   */
  public static final F<Long, Float> Long_Float = new F<Long, Float>() {
    public Float f(final Long l) {
      return (float)(long)l;
    }
  };

  /**
   * A function that converts longs to integers.
   */
  public static final F<Long, Integer> Long_Integer = new F<Long, Integer>() {
    public Integer f(final Long l) {
      return (int)(long)l;
    }
  };

  /**
   * A function that converts longs to shorts.
   */
  public static final F<Long, Short> Long_Short = new F<Long, Short>() {
    public Short f(final Long l) {
      return (short)(long)l;
    }
  };

  // END Long ->

  // BEGIN Short ->

  /**
   * A function that converts shorts to booleans.
   */
  public static final F<Short, Boolean> Short_Boolean = new F<Short, Boolean>() {
    public Boolean f(final Short s) {
      return s != 0;
    }
  };

  /**
   * A function that converts shorts to bytes.
   */
  public static final F<Short, Byte> Short_Byte = new F<Short, Byte>() {
    public Byte f(final Short s) {
      return (byte)(short)s;
    }
  };

  /**
   * A function that converts shorts to characters.
   */
  public static final F<Short, Character> Short_Character = new F<Short, Character>() {
    public Character f(final Short s) {
      return (char)(short)s;
    }
  };

  /**
   * A function that converts shorts to doubles.
   */
  public static final F<Short, Double> Short_Double = new F<Short, Double>() {
    public Double f(final Short s) {
      return (double)(short)s;
    }
  };

  /**
   * A function that converts shorts to floats.
   */
  public static final F<Short, Float> Short_Float = new F<Short, Float>() {
    public Float f(final Short s) {
      return (float)(short)s;
    }
  };

  /**
   * A function that converts shorts to integers.
   */
  public static final F<Short, Integer> Short_Integer = new F<Short, Integer>() {
    public Integer f(final Short s) {
      return (int)(short)s;
    }
  };

  /**
   * A function that converts shorts to longs.
   */
  public static final F<Short, Long> Short_Long = new F<Short, Long>() {
    public Long f(final Short s) {
      return (long)(short)s;
    }
  };

  // END Short
}
