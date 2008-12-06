package fj.test;

import static fj.Bottom.decons;
import fj.F;
import fj.data.List;
import static fj.data.List.asString;
import fj.data.Option;
import static fj.data.Option.some;
import fj.pre.Show;
import static fj.pre.Show.listShow;
import static fj.pre.Show.showS;
import static fj.test.Arg.argShow;

import java.io.StringWriter;
import java.io.PrintWriter;

/**
 * An enumeration of the possible results after checking a property. A <code>CheckResult</code> may
 * be in one of six states:
 * <ol>
 * <li>Passed</li>
 * <li>Proven</li>
 * <li>Falsified</li>
 * <li>Exhausted</li>
 * <li>Exception executing the property</li>
 * <li>Exception generating values to check the property</li>
 * </ol>
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
public final class CheckResult {
  private final R r;
  private final Option<List<Arg<?>>> args;
  private final Option<Throwable> ex;
  private final int succeeded;
  private final int discarded;

  private enum R {
    Passed, Proven, Falsified, Exhausted, PropException, GenException
  }

  private CheckResult(final R r, final Option<List<Arg<?>>> args, final Option<Throwable> ex, final int succeeded, final int discarded) {
    this.r = r;
    this.args = args;
    this.ex = ex;
    this.succeeded = succeeded;
    this.discarded = discarded;
  }

  /**
   * Returns a result that the property has passed.
   *
   * @param succeeded The number of checks that succeeded.
   * @param discarded The number of checks that were discarded.
   * @return A result that the property has passed.
   */
  public static CheckResult passed(final int succeeded, final int discarded) {
    return new CheckResult(R.Passed, Option.<List<Arg<?>>>none(), Option.<Throwable>none(), succeeded, discarded);
  }

  /**
   * Returns a result that the property has been proven.
   *
   * @param args The arguments used to prove the property.
   * @param succeeded The number of checks that succeeded.
   * @param discarded The number of checks that were discarded.
   * @return A result that the property has been proven.
   */
  public static CheckResult proven(final List<Arg<?>> args, final int succeeded, final int discarded) {
    return new CheckResult(R.Proven, some(args), Option.<Throwable>none(), succeeded, discarded);
  }

  /**
   * Returns a result that the property has been falsified.
   *
   * @param args The arguments used to falsify the property.
   * @param succeeded The number of checks that succeeded.
   * @param discarded The number of checks that were discarded.
   * @return A result that the property has been falsified.
   */
  public static CheckResult falsified(final List<Arg<?>> args, final int succeeded, final int discarded) {
    return new CheckResult(R.Falsified, some(args), Option.<Throwable>none(), succeeded, discarded);
  }

  /**
   * Returns a result that the property been exhausted in checking.
   *
   * @param succeeded The number of checks that succeeded.
   * @param discarded The number of checks that were discarded.
   * @return A result that the property has been exhausted in checking.
   */
  public static CheckResult exhausted(final int succeeded, final int discarded) {
    return new CheckResult(R.Exhausted, Option.<List<Arg<?>>>none(), Option.<Throwable>none(), succeeded, discarded);
  }

  /**
   * Returns a result that checking the property threw an exception.
   *
   * @param args The arguments used when the exception was thrown.
   * @param ex The exception that was thrown.
   * @param succeeded The number of checks that succeeded.
   * @param discarded The number of checks that were discarded.
   * @return A result that checking the property threw an exception.
   */
  public static CheckResult propException(final List<Arg<?>> args, final Throwable ex, final int succeeded, final int discarded) {
    return new CheckResult(R.PropException, some(args), some(ex), succeeded, discarded);
  }


  /**
   * Returns a result that generating values to check the property threw an exception.
   *
   * @param ex The exception that was thrown.
   * @param succeeded The number of checks that succeeded.
   * @param discarded The number of checks that were discarded.
   * @return A result that generating values to check the property threw an exception.
   */
  public static CheckResult genException(final Throwable ex, final int succeeded, final int discarded) {
    return new CheckResult(R.GenException, Option.<List<Arg<?>>>none(), some(ex), succeeded, discarded);
  }

  /**
   * Returns <code>true</code> if this result is passed, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this result is passed, <code>false</code> otherwise.
   */
  public boolean isPassed() {
    return r == R.Passed;
  }

  /**
   * Returns <code>true</code> if this result is proven, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this result is proven, <code>false</code> otherwise.
   */
  public boolean isProven() {
    return r == R.Proven;
  }

  /**
   * Returns <code>true</code> if this result is falsified, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this result is falsified, <code>false</code> otherwise.
   */
  public boolean isFalsified() {
    return r == R.Falsified;
  }

  /**
   * Returns <code>true</code> if this result is exhausted, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this result is exhausted, <code>false</code> otherwise.
   */
  public boolean isExhausted() {
    return r == R.Exhausted;
  }


  /**
   * Returns <code>true</code> if this result is an exception during property execution,
   * <code>false</code> otherwise.
   *
   * @return <code>true</code> if this result is an exception during property execution,
   * <code>false</code> otherwise.
   */
  public boolean isPropException() {
    return r == R.PropException;
  }

  /**
   * Returns <code>true</code> if this result is an exception during generating of values for
   * property checking, <code>false</code> otherwise.
   *
   * @return <code>true</code> if this result is an exception during generating of values for
   * property checking, <code>false</code> otherwise.
   */
  public boolean isGenException() {
    return r == R.GenException;
  }

  /**
   * Returns the arguments if the result is one of; proven, falsified or exception during property
   * execution, otherwise, no arguments are returned.
   *
   * @return The arguments if the result is one of; proven, falsified or exception during property
   * execution, otherwise, no arguments are returned.
   */
  public Option<List<Arg<?>>> args() {
    return args;
  }

  /**
   * Returns the execption if the result is one of; exception during property execution or exception
   * during argument value generation, otherwise, no exception are returned.
   *
   * @return The execption if the result is one of; exception during property execution or exception
   * during argument value generation, otherwise, no exception are returned.
   */
  public Option<Throwable> exception() {
    return ex;
  }

  /**
   * Returns the number of succeeded checks of the property in this result.
   *
   * @return The number of succeeded checks of the property in this result.
   */
  public int succeeded() {
    return succeeded;
  }

  /**
   * Returns the number of discarded checks of the property in this result.
   *
   * @return The number of discarded checks of the property in this result.
   */
  public int discarded() {
    return discarded;
  }

  /**
   * A rendering of a check result that summarises in one line.
   *
   * @param sa The rendering of arguments.
   * @return A rendering of a check result that summarises in one line.
   */
  public static Show<CheckResult> summary(final Show<Arg<?>> sa) {
    return showS(new F<CheckResult, String>() {
      private String test(final CheckResult r) {
        return r.succeeded() == 1 ? "test" : "tests";
      }

      private String arguments(final CheckResult r) {
        final List<Arg<?>> args = r.args().some();
        return args.length() == 1 ? "argument: " + sa.showS(args.head()) : "arguments: " + listShow(sa).showS(args); 
      }

      public String f(final CheckResult r) {
        if(r.isProven())
          return "OK, property proven with " + arguments(r);
        else if(r.isPassed())
          return "OK, passed " + r.succeeded() + ' ' + test(r) + (r.discarded() > 0 ? " (" + r.discarded() + " discarded)" : "") + '.';
        else if(r.isFalsified())
          return "Falsified after " + r.succeeded() + " passed " +  test(r) + " with " + arguments(r); 
        else if(r.isExhausted())
          return "Gave up after " + r.succeeded() + " passed " + test(r) + " and " + r.discarded() + " discarded tests.";
        else if(r.isPropException()) {
          final StringWriter sw = new StringWriter();
          final PrintWriter pw = new PrintWriter(sw);
          r.exception().some().printStackTrace(pw);
          return "Exception on property evaluation with " + arguments(r) + System.getProperty("line.separator") + sw;
        } else if(r.isGenException()) {
          final StringWriter sw = new StringWriter();
          final PrintWriter pw = new PrintWriter(sw);
          r.exception().some().printStackTrace(pw);
          return "Exception on argument generation " + System.getProperty("line.separator") + sw;
        } else
          throw decons(r.getClass());
      }
    });
  }

  /**
   * A rendering of a check result that summarises in one line.
   */  
  public static final Show<CheckResult> summary = summary(argShow);

  /**
   * A rendering of a check result that summarises in one line but throws an exception in the result
   * is a failure (falsified, property exception or generator exception).
   */
  public static final Show<CheckResult> summaryEx = summaryEx(argShow);

  /**
   * A rendering of a check result that summarises in one line but throws an exception in the result
   * is a failure (falsified, property exception or generator exception).
   *
   * @param sa The rendering of arguments.
   * @return A rendering of a check result that summarises in one line but throws an exception in
   * the result is a failure (falsified, property exception or generator exception).
   */
  public static Show<CheckResult> summaryEx(final Show<Arg<?>> sa) {
    return showS(new F<CheckResult, String>() {
      public String f(final CheckResult r) {
        final String s = asString(summary(sa).show(r));
        if(r.isProven() || r.isPassed() || r.isExhausted())
          return s;
        else if(r.isFalsified() || r.isPropException() || r.isGenException())
          throw new Error(s);
        else
          throw decons(r.getClass());
      }
    });
  }
}
