/*
 * Copyright (c) 2018, Edmund Noble
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package ztest

import java.lang.String
import scala.Boolean

import scalaz.core.EqClass
import scalaz.Void
import scalaz.Scalaz._
import scalaz.data.IList
import scalaz.effect.IO

trait Suite {
  def run: IO[Void, SuiteOutput]
}

final case class SuiteOutput(lines: IList[String], allSucceeded: Boolean)

object SuiteOutput {
  implicit val eqSuiteOutput: Eq[SuiteOutput] = instanceOf(new EqClass[SuiteOutput] {
    def equal(a: SuiteOutput, b: SuiteOutput): Boolean =
      a.allSucceeded == b.allSucceeded &&
        a.lines === b.lines
  })
}

object Suite {
  def flip[A, B, C](fun: (A, B) => C): (B, A) => C = (b, a) => fun(a, b)
  def reverse[A](as: IList[A]): IList[A] = as.foldLeft(IList.empty[A])(flip(IList.cons))
  def printScope(scope: IList[String]): String = {
    util.fastConcatDelim(reverse(scope), "->")
  }
  def printTest(scope: IList[String], out: TestResult) = out match {
    case _: Success.type => printScope(IList.cons("succeeded\n", scope))
    case _: Failure.type => printScope(IList.cons("failed\n", scope))
    case _: Thrown       => printScope(IList.cons("errored\n", scope))
  }
}
