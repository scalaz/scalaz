package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Show` */
final class ShowOps[F] private[syntax](val self: F)(implicit val F: Show[F]) extends Ops[F] {
  ////
  final def show: Cord = F.show(self)
  final def shows: String = F.shows(self)
  final def print: Unit = Console.print(shows)
  final def println: Unit = Console.println(shows)

  /**
    * 1 internal call, 1 thread call, and ShowOps.traceInternal are the three levels of stack trace offset.
    */
  private val stackOffset = 3

  /**
    * Prints this (self) and returns this
    * @return this (self)
    */
  final def trace0: F = traceInternal(0)
  /**
    * Prints this (self) with a 1 line stack trace for easy locate-ability and returns this
    * @return this (self)
    */
  final def trace1: F = traceInternal(1)
  /**
    * Prints this (self) with a 1 line stack trace for easy locate-ability and returns this (self). Short for "trace1"
    * @return this (self)
    */
  final def trace: F = traceInternal(1)
  /**
    * Prints this (self) with a 2 line stack trace for easy locate-ability and returns this
    * @return this (self)
    */
  final def trace2: F = traceInternal(2)
  /**
    * Prints this (self) with a 3 line stack trace for easy locate-ability and returns this
    * @return this (self)
    */
  final def trace3: F = traceInternal(3)
  /**
    * Prints this (self) with a 4 line stack trace for easy locate-ability and returns this
    * @return this (self)
    */
  final def trace4: F = traceInternal(4)
  /**
    * Prints this (self) with a 5 line stack trace for easy locate-ability and returns this
    * @return this (self)
    */
  final def trace5: F = traceInternal(5)
  /**
    * Prints this (self) with an n line stack trace for easy locate-ability and returns this
    * @param numStackLines The number of lines of stack trace. No stack trace lines if numStackLines is less than or equal to zero.
    * @return this (self)
    */
  final def traceN(numStackLines: Int): F = traceInternal(numStackLines)

  /**
    * Used internally by other trace statements. Prints this (self) with trace.
    * @param numStackLinesIntended The number of lines of stack trace. No stack trace lines if this is less than or equal to zero.
    * @return this (self)
    */
  private final def traceInternal(numStackLinesIntended: Int): F  = {
    val numStackLines = if (numStackLinesIntended > 0) {
      numStackLinesIntended
    } else {
      0
    }
    // adds thread name for easy locatability
    val threadName = Thread.currentThread().getName
    val stackTrace = Thread.currentThread().getStackTrace 
    // format the trace like an exception so the IDE can link with the relevant lines in the source code
    var toPrint = "\"" + shows + "\"" + " in thread " + threadName + ":"
    for (row <- 0 to Math.min(numStackLines - 1, stackTrace.length - 1 - stackOffset)) {
      val lineNumber = stackOffset + row;
      val stackLine = stackTrace(lineNumber);
      toPrint += "\n" + "  at " + stackLine
    }
    toPrint += "\n"
    Console.println(toPrint)
    self
  }
  ////
}

trait ToShowOps  {
  implicit def ToShowOps[F](v: F)(implicit F0: Show[F]) =
    new ShowOps[F](v)

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowOps(v: F): ShowOps[F] = new ShowOps[F](v)(ShowSyntax.this.F)
  
  def F: Show[F]
  ////

  ////
}
