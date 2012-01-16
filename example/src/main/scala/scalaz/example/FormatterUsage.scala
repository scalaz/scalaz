package scalaz.example

import scalaz._
import UnionTypes._
import typelevel._
import Typelevel._
import Formatter._
import formatters.General._
import formatters.String._
import formatters.Numeric._
import formatters.unified.String._

object FormatterUsage extends App {

  def test1(): Unit = {
    println (("test" :: subs(3))("abcdef" :: HNil))
  }

  def test2(): Unit = {
    println (subs(3)("abcdef" :: HNil))
  }

  def test3(): Unit = {
    println ((subs(3) :: "test")("abcdef" :: HNil))
  }

  def test4(): Unit = {
    println((subs(1) :: " " :: subs(2) :: " " :: subs(3))("abcdef" :: "abcdef" :: "abcdef" :: HNil))
  }

  def test5(): Unit = {
    println ((subs(1) :<: subs(2)) format ("abcdef" :: HNil))
  }

  def test6(): Unit = {
    println ((subs(1) :<: " " :: subs(2) :: " " :: subs(3)) format ("abcdef" :: "uvwxyz" :: HNil))
  }

  def test7(): Unit = {
    println (("test" :: subs(3) :: FNil)("abcdef" :: HNil))
  }

  def test8(): Unit = {
    println ((subs(3) :: FNil)("abcdef" :: HNil))
  }

  def test9(): Unit = {
    println ((subs(3) :: "test" :: FNil)("abcdef" :: HNil))
  }

  def test10(): Unit = {
    println ((char() :: FNil)('c'.union[char#D] :: HNil))
  }

  def test11(): Unit = {
    println ((char() :: char() :: FNil)('c'.union[char#D] :: 's'.union[char#D] :: HNil))
  }

  def test12(): Unit = {
    println ((char() :: "#" :: char() :: "#" :: char() :: FNil)('c'.union[char#D] :: (65.toByte).union[char#D] :: (68.toShort).union[char#D] :: HNil))
  }

  def test13(): Unit = {
    println (("#" :: bool() :: "#" :: hex() :: "#" :: str() :: "#" :: FNil)(null :: System.out :: null :: HNil))
  }

  def test14(): Unit = {
    println (("#" :: bool(width = 10, left = true) :: "#" :: hex(width = 10) :: "#" :: str(width = 10, left = true) :: "#" :: FNil)(null :: System.out :: null :: HNil))
  }

  def test15(): Unit = {
    println ((charC() :: charC() :: FNil)('c' :: 's' :: HNil))
  }

  def test16(): Unit = {
    println (("#" :: octalI(width = 10, left = true, indicator=true) :: "#" :: uHexDeciL(width = 10, padding=true) :: "#" :: uScientificF(width = 10, magnitude = 2, padding = true, separators = true, sign = true, space = false, brackets = true) :: "#" :: FNil)(5783 :: (4873.toLong) :: 6847.5897f :: HNil))
  }

  test1()
  test2()
  test3()
  test4()
  test5()
  test6()
  test7()
  test8()
  test9()
  test10()
  test11()
  test12()
  test13()
  test14()
  test15()
  test16()

}


// vim: expandtab:ts=2:sw=2

