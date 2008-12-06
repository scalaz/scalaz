/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2008 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*-------------------------------------------------------------------------*/

package org.scalacheck

import util.{FreqMap,Buildable}
import scala.collection._

/** A property is a generator that generates a property result */
trait Prop {

  import Prop.{Result,Params,Proof,True,False,Exception,Undecided}
  import util.CmdLineParser
  import CmdLineParser.{Success, NoSuccess}

  def apply(prms: Params): Result

  def map(f: Result => Result) = Prop(prms => f(this(prms)))

  def flatMap(f: Result => Prop): Prop = Prop(prms => f(this(prms))(prms))

  def combine(p: Prop)(f: (Result, Result) => Result) =
    for(r1 <- this; r2 <- p) yield f(r1,r2)

  protected def check(prms: Test.Params): Unit = {
    import ConsoleReporter.{testReport, propReport}
    testReport(Test.check(prms, this, propReport))
  }

  /** Convenience method that makes it possible to use a this property
   *  as an application that checks itself on execution */
  def main(args: Array[String]): Unit = CmdLineParser.parseArgs(args) match {
    case Success(params, _) => check(params)
    case e: NoSuccess => 
      println("Incorrect options:"+"\n"+e+"\n")
      CmdLineParser.printHelp
  }

  /** Convenience method that checks this property and reports the
   *  result on the console. Calling <code>p.check</code> is equal
   *  to calling <code>Test.check(p)</code>, but this method does
   *  not return the test statistics. If you need to get the results
   *  from the test, or if you want more control over the test parameters,
   *  use the <code>check</code> methods in <code>Test</code> instead. */
  def check: Unit = Test.check(this)

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate false.  */
  def &&(p: Prop) = combine(p)(_ && _)

  /** Returns a new property that holds if either this
   *  or the given property (or both) hold.  */
  def ||(p: Prop) = combine(p)(_ || _)

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate the same result
   *  as the other property.  */
  def ++(p: Prop): Prop = combine(p)(_ ++ _)

  /** Returns a new property that holds if and only if both this
   *  and the given property generates a result with the same status,
   *  if the status isn't Undecided.  */
  def ==(p: Prop) = this.flatMap(r1 => p.flatMap(r2 =>
    if(r1.status == Undecided || r2.status == Undecided) Prop.undecided
    else if(r1.status == r2.status) Prop.proved
    else Prop.falsified
  ))

  /** Returns a new property that holds if and only if both this
   *  and the given property generates a result with the same status.
   */
  def ===(p: Prop) = this.flatMap(r1 => p.flatMap(r2 =>
    if(r1.status == r2.status) Prop.proved else Prop.falsified
  ))

  override def toString = "Prop"

  /** Put a label on the property to make test reports clearer */
  def label(l: String) = map(_.label(l))

  /** Put a label on the property to make test reports clearer */
  def :|(l: String) = label(l)

  /** Put a label on the property to make test reports clearer */
  def |:(l: String) = label(l)

}

object Prop {

  /** Specifications for the methods in <code>Prop</code> */
  val specification = new Properties("Prop")

  import specification.specify
  import Gen.{value, fail, frequency, elements}
  import Arbitrary._
  import Shrink._


  // Specifications for the Prop class

  specify("Prop.&& Commutativity", (p1: Prop, p2: Prop) =>
    (p1 && p2) === (p2 && p1)
  )
  specify("Prop.&& Exception", (p: Prop) =>
    (p && exception(null)) == exception(null)
  )
  specify("Prop.&& Identity", (p: Prop) =>
    (p && proved) === p
  )
  specify("Prop.&& False", {
    val g = elements(proved,falsified,undecided)
    forAll(g)(p => (p && falsified) == falsified)
  })
  specify("Prop.&& Undecided", {
    val g = elements(proved,undecided)
    forAll(g)(p => (p && undecided) === undecided)
  })
  specify("Prop.&& Right prio", (sz: Int, prms: Params) => {
    val p = proved.map(_.label("RHS")) && proved.map(_.label("LHS"))
    p(prms).labels.contains("RHS")
  })

  specify("Prop.|| Commutativity", (p1: Prop, p2: Prop) =>
    (p1 || p2) === (p2 || p1)
  )
  specify("Prop.|| Exception", (p: Prop) =>
    (p || exception(null)) == exception(null)
  )
  specify("Prop.|| Identity", (p: Prop) =>
    (p || falsified) === p
  )
  specify("Prop.|| True", {
    val g = elements(proved,falsified,undecided)
    forAll(g)(p => (p || proved) == proved)
  })
  specify("Prop.|| Undecided", {
    val g = elements(falsified,undecided)
    forAll(g)(p => (p || undecided) === undecided)
  })

  specify("Prop.++ Commutativity", (p1: Prop, p2: Prop) =>
    (p1 ++ p2) === (p2 ++ p1)
  )
  specify("Prop.++ Exception", (p: Prop) =>
    (p ++ exception(null)) == exception(null)
  )
  specify("Prop.++ Identity 1", {
    val g = elements(falsified,proved,exception(null))
    forAll(g)(p => (p ++ proved) === p)
  })
  specify("Prop.++ Identity 2", (p: Prop) =>
    (p ++ undecided) === p
  )
  specify("Prop.++ False", {
    val g = elements(falsified,proved,undecided)
    forAll(g)(p => (p ++ falsified) === falsified)
  })


  // Types

  type Args = List[Arg]
  type FM = FreqMap[immutable.Set[Any]]

  /** Property parameters */
  case class Params(val genPrms: Gen.Params, val freqMap: FM)

  object Result {
    def apply(st: Status) = new Result(
      st, 
      Nil, 
      immutable.Set.empty[Any], 
      immutable.Set.empty[String]
    )
  }

  /** The result of evaluating a property */
  class Result(
    val status: Status, 
    val args: Args, 
    val collected: immutable.Set[Any], 
    val labels: immutable.Set[String]
  ) {
    def success = status match {
      case True => true
      case Proof => true
      case _ => false
    }

    def failure = status match {
      case False => true
      case Exception(_) => true
      case _ => false
    }

    def addArg(a: Arg) = new Result(status, a::args, collected, labels)

    def collect(x: Any) = new Result(status, args, collected + x, labels)

    def label(l: String) = new Result(status, args, collected, labels + l)

    def &&(r: Result) = (this.status,r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (False,_) => this
      case (_,False) => r

      case (_,Proof) => this
      case (Proof,_) => r

      case (_,True) => this
      case (True,_) => r

      case (Undecided,Undecided) => this
    }

    def ||(r: Result) = (this.status,r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (_,False) => this
      case (False,_) => r

      case (Proof,_) => this
      case (_,Proof) => r

      case (True,_) => this
      case (_,True) => r

      case (Undecided,Undecided) => this
    }

    def ++(r: Result) = (this.status,r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (_, Undecided) => this
      case (Undecided, _) => r

      case (_, Proof) => this
      case (Proof, _) => r

      case (_, True) => this
      case (True, _) => r

      case (False, _) => this
      case (_, False) => r
    }

  }

  sealed trait Status

  /** The property was proved */
  case object Proof extends Status

  /** The property was true */
  case object True extends Status

  /** The property was false */
  case object False extends Status

  /** The property could not be falsified or proved */
  case object Undecided extends Status

  /** Evaluating the property raised an exception */
  sealed case class Exception(e: Throwable) extends Status {
    override def equals(o: Any) = o match {
      case Exception(_) => true
      case _ => false
    }
  }

  def apply(f: Params => Result): Prop = new Prop {
    def apply(prms: Params) = f(prms)
  }

  def apply(r: Result): Prop = Prop(prms => r)


  // Implicit defs

  implicit def extendedBoolean(b: Boolean) = new {
    def ==>(p: => Prop) = Prop.==>(b,p)
  }

  implicit def extendedAny[T](x: => T) = new {
    def imply(f: PartialFunction[T,Prop]) = Prop.imply(x,f)
    def iff(f: PartialFunction[T,Prop]) = Prop.iff(x,f)
    def throws[T <: Throwable](c: Class[T]) = Prop.throws(x, c)
  }

  implicit def propBoolean(b: Boolean): Prop = if(b) proved else falsified


  // Private support functions

  private def provedToTrue(r: Result) = r.status match {
    case Proof => new Result(True, r.args, r.collected, r.labels)
    case _ => r
  }


  // Property combinators

  /** A property that never is proved or falsified */
  lazy val undecided = Prop(Result(Undecided))
  specify("undecided", (prms: Params) => undecided(prms).status == Undecided)

  /** A property that always is false */
  lazy val falsified = Prop(Result(False))
  specify("falsified", (prms: Params) => falsified(prms).status == False)

  /** A property that always is proved */
  lazy val proved = Prop(Result(Proof))
  specify("proved", (prms: Params) => proved(prms).status == Proof)

  /** A property that always is passed */
  lazy val passed = Prop(Result(True))
  specify("passed", (prms: Params) => passed(prms).status == True)

  /** A property that denotes an exception */
  def exception(e: Throwable) = Prop(Result(Exception(e)))
  specify("exception", (prms: Params, e: Throwable) =>
    exception(e)(prms).status == Exception(e))

  /** A property that depends on the generator size */
  def sizedProp(f: Int => Prop): Prop = Prop(prms => f(prms.genPrms.size)(prms))

  /** Implication */
  def ==>(b: => Boolean, p: => Prop): Prop = secure(if (b) p else undecided)

  /** Implication with several conditions */
  def imply[T](x: T, f: PartialFunction[T,Prop]): Prop =
    secure(if(f.isDefinedAt(x)) f(x) else undecided)

  /** Property holds only if the given partial function is defined at
   *  <code>x</code>, and returns a property that holds */
  def iff[T](x: T, f: PartialFunction[T,Prop]): Prop =
    secure(if(f.isDefinedAt(x)) f(x) else falsified)

  /** Combines properties into one, which is true if and only if all the
   *  properties are true */
  def all(ps: Prop*) = if(ps.isEmpty) proved else Prop(prms =>
    ps.map(p => p(prms)).reduceLeft(_ && _)
  )
  specify("all", forAll(Gen.listOf1(value(proved)))(l => all(l:_*)))

  /** Combines properties into one, which is true if at least one of the
   *  properties is true */
  def atLeastOne(ps: Prop*) = if(ps.isEmpty) falsified else Prop(prms =>
    ps.map(p => p(prms)).reduceLeft(_ || _)
  )
  specify("atLeastOne", forAll(Gen.listOf1(value(proved)))(l => atLeastOne(l:_*)))

  /** Existential quantifier */
  def exists[A,P <% Prop](g: Gen[A])(f: A => P): Prop = Prop { prms =>
    g(prms.genPrms) match {
      case None => undecided(prms)
      case Some(x) =>
        val p = secure(f(x))
        val r = p(prms).addArg(Arg(g.label,x,0,x))
        r.status match {
          case True => new Result(Proof, r.args, r.collected, r.labels)
          case False => new Result(Undecided, r.args, r.collected, r.labels)
          case _ => r
        }
    }
  }

  /** Universal quantifier, does not shrink failed test cases. */
  def forAllNoShrink[A,P <% Prop](g: Gen[A])(f: A => P) = Prop { prms =>
    g(prms.genPrms) match {
      case None => undecided(prms)
      case Some(x) =>
        val p = secure(f(x))
        provedToTrue(p(prms)).addArg(Arg(g.label,x,0,x))
    }
  }

  /** Universal quantifier, shrinks failed arguments with given shrink
   *  function */
  def forAllShrink[A, P <% Prop](g: Gen[A],shrink: A => Stream[A])(f: A => P): Prop =
    Prop { prms =>

      /** Returns the first failed result in Left or success in Right */
      def getFirstFailure(xs: Stream[A]): Either[(A,Result),(A,Result)] = {
        assert(!xs.isEmpty, "Stream cannot be empty")
        val results = xs.map { x =>
          val p = secure(f(x))
          (x, provedToTrue(p(prms)))
        }
        results.dropWhile(!_._2.failure).firstOption match {
          case None => Right(results.head)
          case Some(xr) => Left(xr)
        }
      }

      def shrinker(x: A, r: Result, shrinks: Int, orig: A): Result = {
        val xs = shrink(x)
        val res = r.addArg(Arg(g.label,x,shrinks,orig))
        if(xs.isEmpty) res else getFirstFailure(xs) match {
          case Right(_) => res
          case Left((x2,r2)) => shrinker(x2, r2, shrinks+1, orig)
        }
      }

      g(prms.genPrms) match {
        case None => undecided(prms)
        case Some(x) => getFirstFailure(Stream.cons(x, Stream.empty)) match {
          case Right((x,r)) => r.addArg(Arg(g.label,x,0,x))
          case Left((x,r)) => shrinker(x,r,0,x)
        }
      }
   }

  /** Universal quantifier, shrinks failed arguments with the default
   *  shrink function for the type */
  def forAll[T,P](g: Gen[T])(f: T => P)
    (implicit s: Shrink[T], p: P => Prop) = forAllShrink(g, shrink[T])(f)

  /** A property that holds if at least one of the given generators
   *  fails generating a value */
  def someFailing[T](gs: Seq[Gen[T]]) = atLeastOne(gs.map(_ === fail):_*)

  /** A property that holds iff none of the given generators
   *  fails generating a value */
  def noneFailing[T](gs: Seq[Gen[T]]) = all(gs.map(_ !== fail):_*)

  /** A property that holds if the given statement throws an exception
   *  of the specified type */
  def throws[T <: Throwable](x: => Any, c: Class[T]) =
    try { x; falsified } catch { case e if c.isInstance(e) => proved }
  specify("throws", (1/0) throws classOf[ArithmeticException])

  /** Collect data for presentation in test report */
  def collect[T, P <% Prop](f: T => P): T => Prop = t => Prop { prms =>
    val prop = f(t)
    prop(prms).collect(t)
  }

  /** Collect data for presentation in test report */
  def collect[T](t: T)(prop: Prop) = Prop { prms =>
    prop(prms).collect(t)
  }

  /** Collect data for presentation in test report */
  def classify(c: => Boolean, ifTrue: Any)(prop: Prop): Prop =
    if(c) collect(ifTrue)(prop) else collect(())(prop)

  /** Collect data for presentation in test report */
  def classify(c: => Boolean, ifTrue: Any, ifFalse: Any)(prop: Prop): Prop =
    if(c) collect(ifTrue)(prop) else collect(ifFalse)(prop)

  /** Wraps and protects a property */
  def secure[P <% Prop](p: => P): Prop = 
    try { p: Prop } catch { case e => exception(e) }

  /** Converts a function into a universally quantified property */
  def forAll[A1,P] (
    f:  A1 => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1]
  ) = forAllShrink(arbitrary[A1],shrink[A1])(f andThen p)

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,P] (
    f:  (A1,A2) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,P] (
    f:  (A1,A2,A3) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,P] (
    f:  (A1,A2,A3,A4) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,P] (
    f:  (A1,A2,A3,A4,A5) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,A6,P] (
    f:  (A1,A2,A3,A4,A5,A6) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,A6,A7,P] (
    f:  (A1,A2,A3,A4,A5,A6,A7) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6],
    a7: Arbitrary[A7], s7: Shrink[A7]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6, _:A7)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,A6,A7,A8,P] (
    f:  (A1,A2,A3,A4,A5,A6,A7,A8) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],  
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6],
    a7: Arbitrary[A7], s7: Shrink[A7],
    a8: Arbitrary[A8], s8: Shrink[A8]                                        
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6, _:A7, _:A8)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,P] (
    f:  A1 => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1]
  ) = forAllShrink(arbitrary[A1],shrink[A1])(f andThen p)

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,P] (
    f:  (A1,A2) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,A3,P] (
    f:  (A1,A2,A3) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,A3,A4,P] (
    f:  (A1,A2,A3,A4) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,A3,A4,A5,P] (
    f:  (A1,A2,A3,A4,A5) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,A3,A4,A5,A6,P] (
    f:  (A1,A2,A3,A4,A5,A6) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,A3,A4,A5,A6,A7,P] (
    f:  (A1,A2,A3,A4,A5,A6,A7) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6],
    a7: Arbitrary[A7], s7: Shrink[A7]
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6, _:A7)))

  /** Converts a function into a universally quantified property.
   *  @deprecated Use <code>forAll</code> instead. */
  @deprecated def property[A1,A2,A3,A4,A5,A6,A7,A8,P] (
    f:  (A1,A2,A3,A4,A5,A6,A7,A8) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1],
    a2: Arbitrary[A2], s2: Shrink[A2],  
    a3: Arbitrary[A3], s3: Shrink[A3],
    a4: Arbitrary[A4], s4: Shrink[A4],
    a5: Arbitrary[A5], s5: Shrink[A5],
    a6: Arbitrary[A6], s6: Shrink[A6],
    a7: Arbitrary[A7], s7: Shrink[A7],
    a8: Arbitrary[A8], s8: Shrink[A8]                                        
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6, _:A7, _:A8)))
}
