// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.date

/**
 * Utility date functions.
 *
 * @see scala.Predef.String
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Util {
  def approxTimeRender(x: Long) = {
    import StringW._
    
    def weeks = x / (1000 * 60 * 60 * 24 * 7)
    def mweeks = x % (1000 * 60 * 60 * 24 * 7) / (1000 * 60 * 60 * 24)
    def days = x / (1000 * 60 * 60 * 24)
    def mdays = x % (1000 * 60 * 60 * 24) / (1000 * 60 * 60)
    def hours = x / (1000 * 60 * 60)
    def mhours = x % (1000 * 60 * 60) / (1000 * 60)
    def minutes = x / (1000 * 60)
    def mminutes = x % (1000 * 60) / 1000
    def seconds = x / 1000

    def render(x: Long, s: => String, y: Long, units: => String) = x + " " + (s plural x) + (if(y == 0L) "" else " and " + y + " " + (units plural y))

    if(weeks != 0)
      render(weeks, "week", mweeks, "day")
    else if(days != 0)
      render(days, "day", mdays, "hour")
    else if(hours != 0)
      render(hours, "hour", mhours, "minute")
    else if(minutes != 0)
      render(minutes, "minute", mminutes, "second")
    else
      render(seconds, "second", 0L, "")
  }

  def main(args: Array[String]) {
    val a = args(0).toLong
    val b = args(1).toLong
    println(approxTimeRender(b - a))
  }
}
