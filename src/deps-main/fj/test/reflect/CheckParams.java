package fj.test.reflect;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Specifies the check parameters on a {@link fj.test.Property} property with typical defaults.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          <li>$LastChangedBy$</li>
 *          </ul>
 */
@Documented
@Target({ElementType.TYPE, ElementType.FIELD, ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
public @interface CheckParams {
  /**
   * The minimum number of successful tests before a result is reached.
   *
   * @return The minimum number of successful tests before a result is reached.
   */
  int minSuccessful() default 100;

  /**
   * The maximum number of tests discarded because they did not satisfy pre-conditions
   * (i.e. {@link fj.test.Property#implies(boolean, fj.P1)}).
   *
   * @return The maximum number of tests discarded because they did not satisfy pre-conditions
   * (i.e. {@link fj.test.Property#implies(boolean, fj.P1)}).
   */
  int maxDiscarded() default 500;

  /**
   * The minimum size to use for checking.
   *
   * @return The minimum size to use for checking.
   */
  int minSize() default 0;

  /**
   * The maximum size to use for checking.
   *
   * @return The maximum size to use for checking.
   */
  int maxSize() default 100;
}
