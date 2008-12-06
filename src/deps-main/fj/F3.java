package fj;

/**
 * A transformation function of arity-3 from <code>A</code>, <code>B</code> and <code>C</code> to
 * <code>D</code>. This type can be represented using the Java 7 closure syntax.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public interface F3<A, B, C, D> {
  /**
   * Transform <code>A</code>, <code>B</code> and <code>C</code> to <code>D</code>.
   *
   * @param a The <code>A</code> to transform.
   * @param b The <code>B</code> to transform.
   * @param c The <code>C</code> to transform.
   * @return The result of the transformation.
   */
  D f(A a, B b, C c);
}
