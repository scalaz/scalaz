/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck.util

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.util.parsing.input.Position
import scala.collection.Set
import org.scalacheck.Test

object CmdLineParser extends Parsers {

  type Elem = String

  private trait Opt[+T] {
    val default: T
    val names: Set[String]
    val help: String
  }
  private trait Flag extends Opt[Unit]
  private trait IntOpt extends Opt[Int]
  private trait StrOpt extends Opt[String]

  private class OptMap {
    private val opts = new collection.mutable.HashMap[Opt[_], Any]
    def apply(flag: Flag): Boolean = opts.contains(flag)
    def apply[T](opt: Opt[T]): T = opts.get(opt) match {
      case None => opt.default
      case Some(v: T) => v
    }
    def update[T](opt: Opt[T], optVal: T) = opts.update(opt, optVal)
  }

  private object OptMinSuccess extends IntOpt {
    val default = Test.defaultParams.minSuccessfulTests
    val names = Set("minSuccessfulTests", "s")
    val help = "Number of tests that must succeed in order to pass a property"
  }
  private object OptMaxDiscarded extends IntOpt {
    val default = Test.defaultParams.maxDiscardedTests
    val names = Set("maxDiscardedTests", "d")
    val help = 
      "Number of tests that can be discarded before ScalaCheck stops " +
      "testing a property"
  }
  private object OptMinSize extends IntOpt {
    val default = Test.defaultParams.minSize
    val names = Set("minSize", "n")
    val help = "Minimum data generation size"
  }
  private object OptMaxSize extends IntOpt {
    val default = Test.defaultParams.maxSize
    val names = Set("maxSize", "x")
    val help = "Maximum data generation size"
  }
  private object OptWorkers extends IntOpt {
    val default = Test.defaultParams.workers
    val names = Set("workers", "w")
    val help = "Number of threads to execute in parallel for testing"
  }
  private object OptWorkSize extends IntOpt {
    val default = Test.defaultParams.wrkSize
    val names = Set("wrkSize", "z")
    val help = "Amount of work each thread should do at a time"
  }
    
  private val opts = Set[Opt[_]](
    OptMinSuccess, OptMaxDiscarded, OptMinSize, 
    OptMaxSize, OptWorkers, OptWorkSize
  )

  private class ArgsReader(args: Array[String], i: Int) extends Reader[String] {
    val pos = new Position {
      val column = args.subArray(0,i).foldLeft(1)(_ + _.length + 1)
      val line = 1
      val lineContents = args.mkString(" ")
    }
    val atEnd = i >= args.length
    def first = if(atEnd) null else args(i)
    def rest = if(atEnd) this else new ArgsReader(args, i+1)
  }

  private def getOpt(s: String) = {
    if(s == null || s.length == 0 || s.charAt(0) != '-') None
    else opts.find(_.names.contains(s.drop(1)))
  }
  
  private val opt: Parser[Opt[Any]] = accept("option name", {
    case s if getOpt(s).isDefined => getOpt(s).get
  })

  private val strVal: Parser[String] = accept("string", {
    case s if s != null => s
  })

  private val intVal: Parser[Int] = accept("integer", {
    case s if s != null && s.length > 0 && s.forall(_.isDigit) => s.toInt
  })

  private case class OptVal[T](o: Opt[T], v: T)

  private val optVal: Parser[OptVal[Any]] = opt into {
    case o: Flag => success(OptVal(o, ()))
    case o: IntOpt => intVal ^^ (v => OptVal(o, v))
    case o: StrOpt => strVal ^^ (v => OptVal(o, v))
  }

  private val options: Parser[OptMap] = rep(optVal) ^^ { xs =>
    val map = new OptMap
    xs.foreach { case OptVal(o,v) => map(o) = v }
    map
  }

  private val params = options map {
    optMap => Test.Params(
      optMap(OptMinSuccess),
      optMap(OptMaxDiscarded),
      optMap(OptMinSize),
      optMap(OptMaxSize),
      Test.defaultParams.rng,
      optMap(OptWorkers),
      optMap(OptWorkSize)
    )
  }

  def parseArgs(args: Array[String]) = phrase(params)(new ArgsReader(args, 0))

  def printHelp = {
    println("Available options:")
    opts.foreach { opt =>
      println("  " + opt.names.map("-"+_).mkString(", ") + ": " + opt.help)
    }
  }

}
