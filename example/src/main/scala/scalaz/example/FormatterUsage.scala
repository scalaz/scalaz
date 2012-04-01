package scalaz.example

import scalaz._
import std.string._
import UnionTypes._
import typelevel._
import Typelevel._
import Formatter.all._

object FormatterUsage extends App {

  def test1() {
    println (("test" :: subs(3))("abcdef" :: HNil))
  }

  def test2() {
    println (subs(3)("abcdef" :: HNil))
  }

  def test3() {
    println ((subs(3) :: "test")("abcdef" :: HNil))
  }

  def test4() {
    println((subs(1) :: " " :: subs(2) :: " " :: subs(3))("abcdef" :: "abcdef" :: "abcdef" :: HNil))
  }

  def test5() {
    println ((subs(1) :<: subs(2)) format ("abcdef" :: HNil))
  }

  def test6() {
    println ((subs(1) :<: " " :: subs(2) :: " " :: subs(3)) format ("abcdef" :: "uvwxyz" :: HNil))
  }

  def test7() {
    println (("test" :: subs(3) :: FNil)("abcdef" :: HNil))
  }

  def test8() {
    println ((subs(3) :: FNil)("abcdef" :: HNil))
  }

  def test9() {
    println ((subs(3) :: "test" :: FNil)("abcdef" :: HNil))
  }

  def test10() {
    println ((char() :: FNil)('c'.union[char#T] :: HNil))
  }

  def test11() {
    println ((char() :: char() :: FNil)('c'.union[char#T] :: 's'.union[char#T] :: HNil))
  }

  def test12() {
    println ((char() :: "#" :: char() :: "#" :: char() :: FNil)('c'.union[char#T] :: (65.toByte).union[char#T] :: (68.toShort).union[char#T] :: HNil))
  }

  def test13() {
    println (("#" :: bool() :: "#" :: hex() :: "#" :: str() :: "#" :: FNil)(null :: System.out :: null :: HNil))
  }

  def test14() {
    println (("#" :: bool(width = 10, left = true) :: "#" :: hex(width = 10) :: "#" :: str(width = 10, left = true) :: "#" :: FNil)(null :: System.out :: null :: HNil))
  }

  def test15() {
    println ((char().of[Char] :: char().of[Char] :: FNil)('c' :: 's' :: HNil))
  }

  def test16() {
    println (("#" :: octal(width = 10, left = true, indicator = true).of[Int] :: "#" :: uHexDeci(width = 10, padding = true).of[Long] :: "#" :: uScientific(width = 10, magnitude = 2, padding = true, separators = true, sign = true, space = false, brackets = true).of[Float] :: "#" :: FNil)(5783 :: (4873.toLong) :: 6847.5897f :: HNil))
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

