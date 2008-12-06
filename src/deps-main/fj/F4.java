package fj;

/**
 * A transformation function of arity-4 from <code>A</code>, <code>B</code>, <code>C</code> and
 * <code>D</code> to <code>E</code>. This type can be represented using the Java 7 closure syntax.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public interface F4<A, B, C, D, E> {
  /**
   * Transform <code>A</code>, <code>B</code>, <code>C</code> and <code>D</code> to <code>E</code>.
   *
   * @param a The <code>A</code> to transform.
   * @param b The <code>B</code> to transform.
   * @param c The <code>C</code> to transform.
   * @param d The <code>D</code> to transform.
   * @return The result of the transformation.
   */
  E f(A a, B b, C c, D d);
}
