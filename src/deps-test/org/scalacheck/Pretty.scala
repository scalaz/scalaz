/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

sealed trait Pretty[T] {
  def pretty(t: T): String
}

object Pretty {

  import Math.round

  implicit def strBreak(s1: String) = new {
    def /(s2: String) = if(s2 == "") s1 else s1+"\n"+s2
  }

  def apply[T](f: T => String) = new Pretty[T] { def pretty(t: T) = f(t) }

  def pretty[T](t: T)(implicit p: Pretty[T]): String = p.pretty(t)

  def pad(s: String, c: Char, length: Int) = 
    if(s.length >= length) s
    else s + List.make(length-s.length, c).mkString

  def break(s: String, lead: String, length: Int): String =
    if(s.length <= length) s
    else s.substring(0, length) / break(lead+s.substring(length), lead, length)

  def format(s: String, lead: String, trail: String, width: Int) =
    s.lines.map(l => break(lead+l+trail, "  ", width)).mkString("\n")

  implicit lazy val prettyThrowable: Pretty[Throwable] = Pretty { e =>
    e.getClass.getName / e.getStackTrace.map { st =>
      import st._
      getClassName+"."+getMethodName + "("+getFileName+":"+getLineNumber+")"
    }.mkString("\n")
  }

  implicit lazy val prettyArgs = Pretty { args: List[Arg] =>
    if(args.isEmpty) "" else {
      for((a,i) <- args.zipWithIndex) yield {
        val l = if(a.label == "") "ARG_"+i else a.label
        val s = 
          if(a.shrinks == 0) "" 
          else " ("+a.shrinks+" shrinks, original arg: \""+a.origArg+"\")"

        "> "+l+": \""+a.arg+"\""+s
      }
    }.mkString("\n")
  }

  implicit lazy val prettyFreqMap: Pretty[Prop.FM] = Pretty { fm =>
    if(fm.total == 0) "" 
    else {
      "> Collected test data: " / {
        for {
          (xs,r) <- fm.getRatios
          ys = xs - ()
          if !ys.isEmpty
        } yield round(r*100)+"% " + ys.mkString(", ")
      }.mkString("\n")
    }
  }

  implicit lazy val prettyTestRes: Pretty[Test.Result] = Pretty { res =>
    def labels(ls: collection.immutable.Set[String]) = 
      if(ls.isEmpty) "" else "> Labels of failing property: " / {
        ls.map("\"" + _ + "\"")
      }.mkString("\n")
    val s = res.status match {
      case Test.Proved(args) => "OK, proved property."/pretty(args)
      case Test.Passed => "OK, passed "+res.succeeded+" tests."
      case Test.Failed(args, l) =>
        "Falsified after "+res.succeeded+" passed tests."/labels(l)/pretty(args)
      case Test.Exhausted =>
        "Gave up after only "+res.succeeded+" passed tests. " +
        res.discarded+" tests were discarded."
      case Test.PropException(args,e,l) =>
        "Exception raised on property evaluation."/labels(l)/pretty(args)/
        "> Stack trace: "+pretty(e)
      case Test.GenException(e) =>
        "Exception raised on argument generation."/"> Stack trace: "/pretty(e)
    }
    s/pretty(res.freqMap)
  }

}
