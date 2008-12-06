/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck


object Test {

  import util.FreqMap
  import ConsoleReporter.{testReport, propReport}
  import scala.collection.immutable
  import Prop.FM

  private def secure[T](x: => T): Either[T,Throwable] =
    try { Left(x) } catch { case e => Right(e) }


  // Types

  /** Test parameters */
  case class Params(
    minSuccessfulTests: Int, 
    maxDiscardedTests: Int,
    minSize: Int, 
    maxSize: Int, 
    rng: java.util.Random, 
    workers: Int, 
    wrkSize: Int
  )

  /** Test statistics */
  case class Result(status: Status, succeeded: Int, discarded: Int, freqMap: FM) {
    def passed = status match {
      case Passed => true
      case Proved(_) => true
      case _ => false
    }
  }

  /** Test status */
  sealed trait Status

  /** ScalaCheck found enough cases for which the property holds, so the
   *  property is considered correct. (It is not proved correct, though). */
  case object Passed extends Status

  /** ScalaCheck managed to prove the property correct */
  sealed case class Proved(args: List[Arg]) extends Status

  /** The property was proved wrong with the given concrete arguments.  */
  sealed case class Failed(args: List[Arg], labels: Set[String]) extends Status

  /** The property test was exhausted, it wasn't possible to generate enough
   *  concrete arguments satisfying the preconditions to get enough passing
   *  property evaluations. */
  case object Exhausted extends Status

  /** An exception was raised when trying to evaluate the property with the
   *  given concrete arguments. */
  sealed case class PropException(args: List[Arg], e: Throwable, labels: Set[String]) extends Status

  /** An exception was raised when trying to generate concrete arguments
   *  for evaluating the property. */
  sealed case class GenException(e: Throwable) extends Status

  /** Property evaluation callback. Takes number of passed and
   *  discarded tests, respectively */
  type PropEvalCallback = (Int,Int) => Unit

  /** Property evaluation callback. Takes property name, and number of passed
   *  and discarded tests, respectively */
  type NamedPropEvalCallback = (String,Int,Int) => Unit

  /** Test callback. Takes property name, and test results. */
  type TestResCallback = (String,Result) => Unit

  /** Default testing parameters */
  val defaultParams = Params(100,500,0,100,util.StdRand,1,20)
  
  private def assertParams(prms: Params) = {
    import prms._
    if(
      minSuccessfulTests <= 0 || 
      maxDiscardedTests < 0 || 
      minSize < 0 || 
      maxSize < minSize || 
      workers < 1 || 
      (workers > 1 && wrkSize <= 0)
    ) throw new IllegalArgumentException("Invalid test parameters")
  }
    
  
  // Testing functions

  /** Tests a property with the given testing parameters, and returns
   *  the test results. <code>propCallback</code> is a function which is
   *  called each time the property is evaluted. */
  private def checkSingleThread(prms: Params, p: Prop, propCallback: PropEvalCallback): Result =
  {
    assertParams(prms)

    def result(s: Int, d: Int, sz: Float, freqMap: FM): Result = {

      val size: Float = 
        if(s == 0 && d == 0) prms.minSize 
        else sz + ((prms.maxSize-sz)/(prms.minSuccessfulTests-s))

      val propPrms = Prop.Params(Gen.Params(size.round, prms.rng), freqMap)

      secure(p(propPrms)) match {
        case Right(e) => Result(GenException(e), s, d, FreqMap.empty[immutable.Set[Any]])
        case Left(propRes) =>
          val fm =
            if(propRes.collected.isEmpty) freqMap
            else freqMap + propRes.collected
          propRes.status match {
            case Prop.Undecided =>
              if(d+1 >= prms.maxDiscardedTests) Result(Exhausted, s, d+1, fm)
              else { propCallback(s, d+1); result(s, d+1, size, fm) }
            case Prop.True =>
              if(s+1 >= prms.minSuccessfulTests) Result(Passed, s+1, d, fm)
              else { propCallback(s+1, d); result(s+1, d,size, fm) }
            case Prop.Proof =>
              Result(Proved(propRes.args), s+1, d, fm)
            case Prop.False =>
              Result(Failed(propRes.args, propRes.labels), s, d, fm)
            case Prop.Exception(e) =>
              Result(PropException(propRes.args, e, propRes.labels), s, d, fm)
          }
      }
    }

    result(0, 0, prms.minSize, FreqMap.empty[immutable.Set[Any]])
  }

  /** Tests a property with the given testing parameters, and returns
   *  the test results. <code>propCallback</code> is a function which is
   *  called each time the property is evaluted. Uses actors for parallel
   *  test execution, unless <code>workers</code> is less than or equal to 1.
   *  <code>worker</code> specifies how many working actors should be used.
   *  <code>wrkSize</code> specifies how many tests each worker should
   *  be scheduled with. */
  def check(prms: Params, p: Prop, propCallback: PropEvalCallback): Result =
    if(prms.workers <= 1) checkSingleThread(prms, p, propCallback)
    else {
      assert(!p.isInstanceOf[Commands], "Commands cannot be checked multi-threaded")
      assertParams(prms)
      import scala.actors._
      import Actor._
      import prms._

      case class S(status: Status, freqMap: FM, s: Int, d: Int)

      val server = actor {
        var s = 0
        var d = 0
        var size: Float = minSize
        var w = workers
        var res: Result = null
        var fm = FreqMap.empty[immutable.Set[Any]]
        loop { react {
          case 'wrkstop => w -= 1
          case 'get if w == 0 =>
            reply(res)
            exit()
          case 'params => if(res != null) reply() else {
            reply((s,d,size,fm))
            size += wrkSize*((maxSize-size)/(minSuccessfulTests-s))
          }
          case S(status, freqMap, sDelta, dDelta) if res == null =>
            s += sDelta
            d += dDelta
            fm ++= freqMap
            if(res != null) res = Result(status, s, d, fm)
            else {
              if(s >= minSuccessfulTests) res = Result(Passed,s,d,fm)
              else if(d >= maxDiscardedTests) res = Result(Exhausted,s,d,fm)
              else propCallback(s,d)
            }
        }}
      }

      def worker = actor {
        var stop = false
        while(!stop) (server !? 'params) match {
          case (s: Int, d: Int, sz: Float, freqMap: FM) =>
            var s2 = s
            var d2 = d
            var size = sz
            var i = 0
            var fm = freqMap
            var status: Status = null
            while(status == null && i < wrkSize) {
              val propPrms = Prop.Params(Gen.Params(size.round, rng), fm)
              secure(p(propPrms)) match {
                case Right(e) => status =  GenException(e)
                case Left(propRes) =>
                  if(!propRes.collected.isEmpty) fm += propRes.collected
                  propRes.status match {
                    case Prop.Undecided =>
                      d2 += 1
                      if(d2 >= maxDiscardedTests) status = Exhausted
                    case Prop.True =>
                      s2 += 1
                      if(s2 >= minSuccessfulTests) status = Passed
                    case Prop.Proof =>
                      s2 += 1
                      status = Proved(propRes.args)
                    case Prop.False =>
                      status = Failed(propRes.args, propRes.labels)
                    case Prop.Exception(e) =>
                      status = PropException(propRes.args, e, propRes.labels)
                  }
              }
              size += ((maxSize-size)/(minSuccessfulTests-s2))
              i += 1
            }
            server ! S(status, fm--freqMap, s2-s, d2-d)
          case _ => stop = true
        }
        server ! 'wrkstop
      }

      for(_ <- 1 to workers) worker
      (server !? 'get).asInstanceOf[Result]
    }

  /** Tests a property with the given testing parameters, and returns
   *  the test results. */
  def check(prms: Params, p: Prop): Result = check(prms,p, (s,d) => ())

  /** Tests a property and prints results to the console. The
   *  <code>maxDiscarded</code> parameter specifies how many
   *  discarded tests that should be allowed before ScalaCheck
   *  gives up. */
  def check(p: Prop, maxDiscarded: Int): Result = {
    val Params(minSuccessfulTests, _, minSize, maxSize, rng, workers, wrkSize) = defaultParams
    val params = Params(minSuccessfulTests,maxDiscarded,minSize,maxSize,rng,workers,wrkSize)
    testReport(check(params, p, propReport))
  }

  /** Tests a property and prints results to the console */
  def check(p: Prop): Result = testReport(check(defaultParams, p, propReport))

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. <code>f</code> is a function which is called each
   *  time a property is evaluted. <code>g</code> is a function called each
   *  time a property has been fully tested. */
  def checkProperties(ps: Properties, prms: Params,
    propCallback: NamedPropEvalCallback, testCallback: TestResCallback
  ): Seq[(String,Result)] = ps.properties.map { case (pName,p) =>
    val res = check(prms,p,propCallback(pName,_,_))
    testCallback(pName,res)
    (pName,res)
  }

  /** Tests all properties with the given testing parameters, and returns
   *  the test results. */
  def checkProperties(ps: Properties, prms: Params): Seq[(String,Result)] =
    checkProperties(ps, prms, (n,s,d) => (), (n,s) => ())

  /** Tests all properties with default testing parameters, and returns
   *  the test results. The results are also printed on the console during
   *  testing. */
  def checkProperties(ps: Properties): Seq[(String,Result)] =
    checkProperties(ps, defaultParams, propReport, testReport)
}
