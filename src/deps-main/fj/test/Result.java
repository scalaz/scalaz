package fj.test;

import fj.F;
import fj.P1;
import fj.data.List;
import fj.data.Option;
import static fj.data.Option.none;
import static fj.data.Option.some;

/**
 * The result of evaluating a property.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
public final class Result {
  private final Option<List<Arg<?>>> args;
  private final R r;
  private final Option<Throwable> t;

  private enum R {
    Unfalsified, Falsified, Proven, Exception, NoResult
  }

  private Result(final Option<List<Arg<?>>> args, final R r, final Option<Throwable> t) {
    this.args = args;
    this.r = r;
    this.t = t;
  }

  /**
   * Returns the potential arguments associated with this result. This will only have a value, if
   * and only if {@link #noResult(Option) !noResult()} holds.
   *
   * @return The potential arguments associated with this result.
   */
  public Option<List<Arg<?>>> args() {
    return args;
  }

  /**
   * Returns the potential exception associated with this result. This will only have a value if and
   * only if this result is an exception result.
   *
   * @return The potential exception associated with this result.
   */
  public Option<Throwable> exception() {
    return t;
  }

  /**
   * Returns <code>true</code> if this result is unfalsified; otherwise, <code>false</code>.
   *
   * @return <code>true</code> if this result is unfalsified; otherwise, <code>false</code>.
   */
  public boolean isUnfalsified() {
    return r == R.Unfalsified;
  }

  /**
   * Returns <code>true</code> if this result is falsified; otherwise, <code>false</code>.
   *
   * @return <code>true</code> if this result is falsified; otherwise, <code>false</code>.
   */
  public boolean isFalsified() {
    return r == R.Falsified;
  }

  /**
   * Returns <code>true</code> if this result is proven; otherwise, <code>false</code>.
   *
   * @return <code>true</code> if this result is proven; otherwise, <code>false</code>.
   */
  public boolean isProven() {
    return r == R.Proven;
  }

  /**
   * Returns <code>true</code> if this result is an exception; otherwise, <code>false</code>.
   *
   * @return <code>true</code> if this result is an exception; otherwise, <code>false</code>.
   */
  public boolean isException() {
    return r == R.Exception;
  }

  /**
   * Returns <code>true</code> if this result is no result; otherwise, <code>false</code>.
   *
   * @return <code>true</code> if this result is no result; otherwise, <code>false</code>.
   */
  public boolean isNoResult() {
    return r == R.NoResult;
  }

  /**
   * Returns <code>true</code> if this result is falsified or an exception; otherwise,
   * <code>false</code>.
   *
   * @return <code>true</code> if this result is falsified or an exception; otherwise,
   * <code>false</code>.
   */
  public boolean failed() {
    return isFalsified() || isException();
  }

  /**
   * Returns <code>true</code> if this result is unfalsified or proven; otherwise,
   * <code>false</code>.
   *
   * @return <code>true</code> if this result is unfalsified or proven; otherwise,
   * <code>false</code>.
   */
  public boolean passed() {
    return isUnfalsified() || isProven();
  }

  /**
   * If this result is proven, alter it to be unfalsified with the same arguments; otherwise, return
   * <code>this</code>.
   *
   * @return If this result is proven, alter it to be unfalsified with the same arguments;
   * otherwise, return <code>this</code>.
   */
  public Result provenAsUnfalsified() {
    if(isProven())
      return unfalsified(args.some());
    else
      return this;
  }

  /**
   * Adds an argument to this result.
   *
   * @param a The argument to add.
   * @return A result with the new argument.
   */
  public Result addArg(final Arg<?> a) {
    final F<Arg<?>, F<List<Arg<?>>, List<Arg<?>>>> cons = List.cons();
    return new Result(args.map(cons.f(a)), r, t);
  }

  /**
   * Returns a potential result for this result. This will have a value if this result is
   * {@link #noResult(Option) !noResult()}.
   *
   * @return A potential result for this result.
   */
  public Option<Result> toOption() {
    if(isNoResult())
      return none();
    else
      return some(this);
  }

  /**
   * Returns a result from the given potential result.
   *
   * @param r The potential result.
   * @return The result that may be {@link #noResult(Option) noResult()}.
   */
  public static Result noResult(final Option<Result> r) {
    return r.orSome(new P1<Result>() {
      public Result _1() {
        return noResult();
      }
    });
  }

  /**
   * Returns a result representing no result.
   *
   * @return A result representing no result.
   */
  public static Result noResult() {
    return new Result(Option.<List<Arg<?>>>none(), R.NoResult, Option.<Throwable>none());
  }

  /**
   * Returns an unfalsified result.
   *
   * @param args The arguments used during the failure of falsification.
   * @return An unfalsified result.
   */
  public static Result unfalsified(final List<Arg<?>> args) {
    return new Result(some(args), R.Unfalsified, Option.<Throwable>none());
  }

  /**
   * Returns a falsified result.
   *
   * @param args The arguments used during falsification.
   * @return A falsified result.
   */
  public static Result falsified(final List<Arg<?>> args) {
    return new Result(some(args), R.Falsified, Option.<Throwable>none());
  }

  /**
   * Returns a proven result.
   *
   * @param args The arguments used during proof.
   * @return A proven result.
   */
  public static Result proven(final List<Arg<?>> args) {
    return new Result(some(args), R.Proven, Option.<Throwable>none());
  }

  /**
   * Returns an exception result.
   *
   * @param args The arguments used when the exception occurred.
   * @param t The exception that occurred.
   * @return A exception result.
   */
  public static Result exception(final List<Arg<?>> args, final Throwable t) {
    return new Result(some(args), R.Exception, some(t));
  }
}
