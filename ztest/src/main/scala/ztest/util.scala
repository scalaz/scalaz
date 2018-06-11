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

package scalaz.test

import scala._, scala.Predef._

import java.lang.{Integer, StringBuilder}

import scalaz.data.{IList, Maybe2}

private[test] object util {
  def fastConcat(strs: IList[String]): String =
    fastConcatDelim(strs, "")

  def fastConcatDelim(strs: IList[String], delim: String): String = {
    var totalLength: Integer = Int.box(0)
    var numStrs: Integer = Int.box(0)
    def go(cursor: IList[String]): Unit = IList.uncons(cursor) match {
      case Maybe2.Just2(s, ss) =>
        totalLength = Int.box(totalLength.intValue() + s.length + delim.length)
        numStrs = Int.box(numStrs.intValue() + 1)
        go(ss)
      case Maybe2.Empty2() =>
    }
    go(strs)
    val sb = new StringBuilder(totalLength + (numStrs * delim.length))
    def printOutDelimited(cursor: IList[String]): Unit = IList.uncons(cursor) match {
      case Maybe2.Just2(s, ss) =>
        sb.append(delim)
        sb.append(s)
        printOutDelimited(ss)
      case Maybe2.Empty2() =>
    }
    IList.uncons(strs) match {
      case Maybe2.Just2(s, ss) => sb.append(s); printOutDelimited(ss)
    }

    sb.toString
  }
}
