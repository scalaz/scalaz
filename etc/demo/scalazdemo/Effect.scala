package scalazdemo

import scalaz.Effect._

/*
unless false
when true
*/
object Effect {
  def effectDemoes {
    // unless
    println("unless false") unless false
    println("unless true") unless true

    // when
    println("when false") when false
    println("when true") when true  
  }

  def main(args: Array[String]) {
    effectDemoes
  }
}
