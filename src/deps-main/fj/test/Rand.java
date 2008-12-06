package fj.test;

import fj.F;
import fj.data.Option;
import static fj.data.Option.some;

import static java.lang.Math.max;
import static java.lang.Math.min;
import java.util.Random;

/**
 * A random number generator.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
public final class Rand {
  private final F<Option<Long>, F<Integer, F<Integer, Integer>>> f;
  private final F<Option<Long>, F<Double, F<Double, Double>>> g;

  private Rand(final F<Option<Long>, F<Integer, F<Integer, Integer>>> f, final F<Option<Long>, F<Double, F<Double, Double>>> g) {
    this.f = f;
    this.g = g;
  }

  /**
   * Randomly chooses a value between the given range (inclusive).
   *
   * @param seed The seed to use for random generation.
   * @param from The minimum value to choose.
   * @param to   The maximum value to choose.
   * @return A random value in the given range.
   */
  public int choose(final long seed, final int from, final int to) {
    return f.f(some(seed)).f(from).f(to);
  }

  /**
   * Randomly chooses a value between the given range (inclusive).
   *
   * @param from The minimum value to choose.
   * @param to   The maximum value to choose.
   * @return A random value in the given range.
   */
  public int choose(final int from, final int to) {
    return f.f(Option.<Long>none()).f(from).f(to);
  }

  /**
   * Randomly chooses a value between the given range (inclusive).
   *
   * @param seed The seed to use for random generation.
   * @param from The minimum value to choose.
   * @param to   The maximum value to choose.
   * @return A random value in the given range.
   */
  public double choose(final long seed, final double from, final double to) {
    return g.f(some(seed)).f(from).f(to);
  }

  /**
   * Randomly chooses a value between the given range (inclusive).
   *
   * @param from The minimum value to choose.
   * @param to   The maximum value to choose.
   * @return A random value in the given range.
   */
  public double choose(final double from, final double to) {
    return g.f(Option.<Long>none()).f(from).f(to);
  }

  /**
   * Gives this random generator a new seed.
   *
   * @param seed The seed of the new random generator.
   * @return A random generator with the given seed.
   */
  public Rand reseed(final long seed) {
    return new Rand(new F<Option<Long>, F<Integer, F<Integer, Integer>>>() {
      public F<Integer, F<Integer, Integer>> f(final Option<Long> old) {
        return new F<Integer, F<Integer, Integer>>() {
          public F<Integer, Integer> f(final Integer from) {
            return new F<Integer, Integer>() {
              public Integer f(final Integer to) {
                return f.f(some(seed)).f(from).f(to);
              }
            };
          }
        };
      }
    }, new F<Option<Long>, F<Double, F<Double, Double>>>() {
      public F<Double, F<Double, Double>> f(final Option<Long> old) {
        return new F<Double, F<Double, Double>>() {
          public F<Double, Double> f(final Double from) {
            return new F<Double, Double>() {
              public Double f(final Double to) {
                return g.f(some(seed)).f(from).f(to);
              }
            };
          }
        };
      }
    });
  }

  /**
   * Constructs a random generator from the given functions that supply a range to produce a
   * result.
   *
   * @param f The integer random generator.
   * @param g The floating-point random generator.
   * @return A random generator from the given functions that supply a range to produce a result.
   */
  public static Rand rand(final F<Option<Long>, F<Integer, F<Integer, Integer>>> f, final F<Option<Long>, F<Double, F<Double, Double>>> g) {
    return new Rand(f, g);
  }


  private static final F<Long, Random> fr = new F<Long, Random>() {
    public Random f(final Long x) {
      return new Random(x);
    }
  };

  /**
   * A standard random generator that uses {@link Random}.
   */
  public static final Rand standard = new Rand(new F<Option<Long>, F<Integer, F<Integer, Integer>>>() {
    public F<Integer, F<Integer, Integer>> f(final Option<Long> seed) {
      return new F<Integer, F<Integer, Integer>>() {
        public F<Integer, Integer> f(final Integer from) {
          return new F<Integer, Integer>() {
            public Integer f(final Integer to) {
              final int f = min(from, to);
              final int t = max(from, to);
              return f + seed.map(fr).orSome(new Random()).nextInt(t - f + 1);
            }
          };
        }
      };
    }
  }, new F<Option<Long>, F<Double, F<Double, Double>>>() {
    public F<Double, F<Double, Double>> f(final Option<Long> seed) {
      return new F<Double, F<Double, Double>>() {
        public F<Double, Double> f(final Double from) {
          return new F<Double, Double>() {
            public Double f(final Double to) {
              final double f = min(from, to);
              final double t = max(from, to);
              return seed.map(fr).orSome(new Random()).nextDouble() * (t - f) + f;
            }
          };
        }
      };
    }
  });
}
