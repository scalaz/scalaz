package fj;

/**
 * The unit type which has only one value.
 *
 * @version %build.number%<br>
 *          <ul>
 *          <li>$LastChangedRevision$</li>
 *          <li>$LastChangedDate$</li>
 *          </ul>
 */
public final class Unit {
  private static final Unit u = new Unit();

  private Unit() {

  }

  /**
   * The only value of the unit type.
   *
   * @return The only value of the unit type.
   */
  public static Unit unit() {
    return u;
  }
}
