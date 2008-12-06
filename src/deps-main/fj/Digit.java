package fj;

import fj.data.Option;
import static fj.data.Option.some;
import static fj.data.Option.none;

/**
 * The digits zero to nine.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public enum Digit {
  /**
   * Zero.
   */
  _0,

  /**
   * One.
   */
  _1,

  /**
   * Two.
   */
  _2,

  /**
   * Three.
   */
  _3,

  /**
   * Four.
   */
  _4,

  /**
   * Five.
   */
  _5,

  /**
   * Six.
   */
  _6,

  /**
   * Seven.
   */
  _7,

  /**
   * Eight.
   */
  _8,

  /**
   * Nine.
   */
  _9;

  /**
   * Converts this digit to a long.
   *
   * @return A long for this digit.
   */
  public long toLong() {
    switch(this) {
      case _0: return 0L;
      case _1: return 1L;
      case _2: return 2L;
      case _3: return 3L;
      case _4: return 4L;
      case _5: return 5L;
      case _6: return 6L;
      case _7: return 7L;
      case _8: return 8L;
      default: return 9L;
    }
  }

  /**
   * Converts this digit to a character.
   *
   * @return A character for this digit.
   */
  public char toChar() {
    switch(this) {
      case _0: return '0';
      case _1: return '1';
      case _2: return '2';
      case _3: return '3';
      case _4: return '4';
      case _5: return '5';
      case _6: return '6';
      case _7: return '7';
      case _8: return '8';
      default: return '9';
    }
  }

  /**
   * Converts the right-most digit in the given long value to a digit.
   *
   * @param i The long to convert.
   * @return The right-most digit in the given long value as a digit.
   */
  public static Digit fromLong(final long i) {
    long x = Math.abs(i) % 10L;
    return x == 0L ? _0 :
           x == 1L ? _1 :
           x == 2L ? _2 :
           x == 3L ? _3 :
           x == 4L ? _4 :
           x == 5L ? _5 :
           x == 6L ? _6 :
           x == 7L ? _7 :
           x == 8L ? _8 :
           _9;
  }

  /**
   * Converts the given character in the given long value to a digit.
   *
   * @param c The character to convert.
   * @return The character in the given long value as a digit.
   */
  public static Option<Digit> fromChar(final char c) {
    switch(c) {
      case '0': return some(_0);
      case '1': return some(_1);
      case '2': return some(_2);
      case '3': return some(_3);
      case '4': return some(_4);
      case '5': return some(_5);
      case '6': return some(_6);
      case '7': return some(_7);
      case '8': return some(_8);
      case '9': return some(_9);
      default : return none();
    }
  }

  /**
   * First-class conversion from digit to a long.
   */
  public static final F<Digit, Long> toLong = new F<Digit, Long>() {
    public Long f(final Digit d) {
      return d.toLong();
    }
  };

  /**
   * First-class conversion from a long to a digit.
   */
  public static final F<Long, Digit> fromLong = new F<Long, Digit>() {
    public Digit f(final Long i) {
      return fromLong(i);
    }
  };

  /**
   * First-class conversion from a digit to a character.
   */
  public static final F<Digit, Character> toChar = new F<Digit, Character>() {
    public Character f(final Digit d) {
      return d.toChar();
    }
  };

  /**
   * First-class conversion from a character to a digit.
   */
  public static final F<Character, Option<Digit>> fromChar = new F<Character, Option<Digit>>() {
    public Option<Digit> f(final Character c) {
      return fromChar(c);
    }
  };
}
