package scalaz.example

/*
 A rather contrived example which shows how ReaderWriterStateT could be used.
 @author stew@vireo.org / stew@helloreverb.com

 We create a simple langauage named CAB
 Strings in the CAB language is a series of A, B, or C tokens followed by a EOF:
 <cab> ::= 'C' | 'A' | 'B'
 <string> ::= <cab> <string> | '.'


 So valid strings in the langauge would be:
 "."
 "A."
 "B."
 "ABC."
 "AAAABBCABC."
 */

import scalaz._
import Scalaz._

sealed trait Token
case object C extends Token
case object A extends Token
case object B extends Token

object Token {

  implicit val euqalsRef: Equal[Token] = Equal.equalRef
  implicit val showTok: Show[Token] = new Show[Token] {
    override def show(t: Token) = t match {
      case A => Cord("A")
      case B => Cord("B")
      case C => Cord("C")
    }
  }
}


object CABRunLengthEncoder {
  /*
    A run-length encoder for CAB, a token T can be prefixed by a number N to stand for "N Ts in a row":

   <cab> ::= 'C' | 'A' | "B'
   <tok> ::= <cab> | <integer> <cab>
   <compressed> ::= <tok> <compressed> | "."

   so the string "CAAAAAB." could be "compressed" any of the following ways:
   "CAAAAAB."
   "C5AB."
   "C3AAAB."
   "1C5CB."
   */

  // The State we will carry in the Monad during our computation
  case class RunLengthState(
    lastToken: Option[Token],  // what is the last char we've seen
    length: Int,               // how many of that char have we seen
    input: List[Token]         // remaining input
  ) {
    def incLength = this.copy(length = length + 1)
    def newToken(t: Token) = RunLengthState(Some(t), 0, input)
    def uncons: (Token, RunLengthState) = (input.head, this.copy(input = input.tail))
  }
  object RunLengthState {
    def initial(input: List[Token]) = RunLengthState(None,0, input)
  }

  /**
    * the configuration of our encoder, this will be used in the
    * Reader part of our RWST    */
  case class RunLengthConfig(
    /**
      * if we are emitting less than minRun tokens, just emit them as
      * individual tokens instead of as a run of tokens
      */
    minRun: Int
  )

  import Token._
  import Free.Trampoline

  type RunLength[A] = ReaderWriterStateT[Trampoline, RunLengthConfig, Cord, RunLengthState, A]

  // At its essence the RWST monad transformer is a wrap around a function with the following shape:
  // (ReaderType, StateType) => Monad[WriterType, Result, StateType]
  // we can directly wrap a function of this shape:
  /**
    *  read a token from the input
    */
  val readToken: RunLength[Token] = ReaderWriterStateT { (config: RunLengthConfig, oldState: RunLengthState) =>
    val (nextTok, newState) = oldState.uncons
    Applicative[Trampoline].point((Monoid[Cord].zero, nextTok, newState))
  }

  // Bring the RWST syntax into scope.
  // This will bring into scope methods which return things of type RunLength[_].
  // example syntax methods:
  // get    -- gets the current state
  // put    -- replaces the current state
  // modify -- alter the current state
  // tell   -- append to the writer
  // ask    -- read from the reader
  val rle = ReaderWriterStateT.rwstMonad[Trampoline, RunLengthConfig, Cord, RunLengthState]
  import rle._

  /**
    * with the above syntax imported, we can perform the same
    * computation as above, but use a for comprehension
    */
  val readToken2: RunLength[Token] =
    for {
      oldState <- get            // fetch the current state

                                // take a token off the input, getting
                                // a token and a new state
      (nextTok, newState) = oldState.uncons
      _ <- put(newState)         // store the new state
    } yield nextTok             // return the token

  /**
    have we exhausted the input?
    */
  def done: RunLength[Boolean] =
    get flatMap { state =>
      if(state.input.isEmpty)
        // we have, better emit whatever tokens are stored in the
        // current state
        emit as true
      else
        point(false)
    }

  /**
    * put output on the writer
    */
  def writeOutput(token: Token, length: Int, minRun: Int): RunLength[Unit] =
    if(length <= minRun)
      tell(Monoid[Cord].multiply(token.show, length))
    else
      tell(length.show ++ token.show)


  /**
    emit the lastToken
    */
  def emit: RunLength[Unit] =
    for {
       state <- get
      config <- ask
           _ <- state.lastToken.cata(none = point(()),  // nothing to emit
                                    some = writeOutput(_, state.length, config.minRun))
    } yield ()


  /**
    emit tokens if the next input token is different than the last
    */
  def maybeEmit: RunLength[Unit] =
    for {
      state <- get
       next <- readToken
           _ <- { if(state.lastToken.map(_ == next) getOrElse(false))
                 // Same token as last, so we just increment our counter
                 modify(_.incLength)
               else
                 // its a new token, so emit the previous, then change
                 // the token in the state.
                 // *> here chains two actions, ignoring the output of
                 // the first (which is unit in this case)
                 emit *> put(state.newToken(next))
          }
    } yield ()

  def encode(minRun: Int, input: List[Token]): String = {
    val config = RunLengthConfig(minRun)
    val initialState = RunLengthState.initial(input)
    val (output, result, finalState) = untilM_(maybeEmit, done).run(config, initialState).run
    output.shows
  }
}

object ReaderWriterStateTUsage extends App {

  val inputTokens = List(A,B,C,A,A,B,B,B,B,C,A,A,A,C,C,C,C,B)
  val encoded = CABRunLengthEncoder.encode(2, inputTokens)

  println("encoded " + inputTokens.mkString  + " as " + encoded)
}
