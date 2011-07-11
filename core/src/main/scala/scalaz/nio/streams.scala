//package scalaz.nio
//
//import scalaz._
//import effect.IO
//import concurrent.Promise
//import iteratee._
//import java.nio.ByteBuffer
//import java.nio.CharBuffer
//import java.nio.channels._
//import java.net.{InetSocketAddress, InetAddress}
//import spi.SelectorProvider
//import collection.JavaConversions._
//import java.nio.charset.{CodingErrorAction, CharsetDecoder}
//import java.nio.charset.Charset
//import Scalaz._
//
//object streams {
//
//  def buffer[C : ClassManifest : EmptyChunk, M[_],A](n : Int) = new Enumeratee[C, Array[C], M, A] {
//    def apply(i: Iteratee[Array[C], M, A])(implicit m: Monad[M]): Iteratee[C, M, Iteratee[Array[C], M, A]] = {
//      def step(cur: Array[C], idx: Int, i: Iteratee[Array[C], M, A]): (Input[C] => Iteratee[C, M, Iteratee[Array[C], M, A]]) = {
//        case e@EOF(Some(err)) => iteratees.Failure(err, e)
//        case e@EOF(None) => Done(i, e)
//        case e@Chunk(el) =>
//          cur(idx) = el
//          if (idx + 1 != n) Cont(step(cur, idx + 1, i))
//          else {
//            val result = FlattenI(i <<: enumInput(Chunk(cur)))
//            FlattenI(result.fold[Iteratee[C, M, Iteratee[Array[C], M, A]]](
//              cont = _ => Cont(step(Array.ofDim(n), 0, result)).pure,
//              done = (_, _) => Done(result, EmptyChunk[C]).pure,
//              error = (msg, _) => iteratees.Failure(msg, EmptyChunk[C]).pure
//            ))
//          }
//      }
//      Cont(step(Array.ofDim(n), 0, i))
//    }
//  }
//
//  def decode[M[_],A](charset :  Charset = Charset.defaultCharset) = decodeWithEncoder[M,A](
//    charset.newDecoder().onMalformedInput(
//      CodingErrorAction.REPLACE).onUnmappableCharacter(
//        CodingErrorAction.REPLACE))
//
//  private def copyToBuf(in : ByteBuffer, out : ByteBuffer) : Boolean = {
//    // If output is limited, compact it so we can continue filling.
//    if (out.limit() == out.capacity()) {
//      out.compact();
//      out.limit(out.position());
//      out.position(0);
//    }
//    val read = math.min(in.remaining, out.remaining)
//    in.get(out.array, out.arrayOffset, read)
//    out.position(out.position + read)
//    // TOOD - update the out pointer...
//    in.remaining > 0
//  }
//  def decodeWithEncoder[M[_], A](decoder : CharsetDecoder) = new Enumeratee[ByteBuffer,CharBuffer, M,A] {
//    def apply(i : Iteratee[CharBuffer,M,A])(implicit m : Monad[M]) : Iteratee[ByteBuffer, M, Iteratee[CharBuffer,M,A]] = {
//      val buf = ByteBuffer.allocate(8 * 1024) // Allocate a buffer to hold data while we decode.
//      val charbuf = CharBuffer.allocate(1024);
//      def step(current : Iteratee[CharBuffer,M,A]) : Input[ByteBuffer] => Iteratee[ByteBuffer, M, Iteratee[CharBuffer, M, A]] = {
//        case i1 @ Chunk(in) =>
//          copyToBuf(in, buf)
//          buf.flip()
//          charbuf.clear()
//          val result = decoder.decode(buf, charbuf, false)
//          if (result.isError) {
//            iteratees.Failure(result.toString, i1)
//          } else {
//            // Send to iteratee...
//            charbuf.flip()
//            val next = FlattenI(current <<: enumInput(Chunk(charbuf)))
//            if (in.hasRemaining) {
//              step(next)(Chunk(in))
//            } else FlattenI(next.fold[Iteratee[ByteBuffer, M, Iteratee[CharBuffer, M, A]]](
//              done = (_, _) => Done(next, EmptyChunk.apply[ByteBuffer]).pure,
//              error = (msg, _) => iteratees.Failure(msg, i1).pure,
//              cont = (_) => Cont(step(next)).pure
//            ))
//          }
//        case e @ EOF(None) =>
//          charbuf.clear()
//          val result = decoder.decode(buf, charbuf, true)
//          decoder.flush(charbuf)
//          charbuf.flip()
//          if (result.isError)
//            iteratees.Failure(result.toString, e)
//          else {
//            charbuf.flip()
//            val result = FlattenI(FlattenI(current <<: enumInput(Chunk(charbuf))) <<: enumEof)
//            Done(result, e)
//          }
//        case e @ EOF(Some(err)) => iteratees.Failure(err, e)
//      }
//      Cont(step(i))
//    }
//  }
//
//  def lines[M[_], A] = new Enumeratee[CharBuffer, String, M, A] {
//    override def apply(i : Iteratee[String,M,A])(implicit m : Monad[M]) : Iteratee[CharBuffer, M, Iteratee[String,M,A]] = {
//      def step(buf : Rope[Char],cur : Iteratee[String, M, A]) : Input[CharBuffer] => Iteratee[CharBuffer, M, Iteratee[String,M,A]] = {
//        case e @ Chunk(chars) =>
//          val realchars = chars.array.view.drop(chars.arrayOffset).take(chars.remaining)
//          val idx = realchars.indexWhere( c => c == '\r' || c == '\n')
//          if (idx == -1) Cont(step(buf ++ Rope.fromArray(realchars.toArray), cur))
//          else {
//            val (now, next) = realchars.splitAt(idx)
//            // TODO - Check next to see if we had \r\n and remove one more char from next....
//            val nextInput = Chunk(CharBuffer.wrap(next.drop(1).toArray))
//            val curInput = Chunk(String.valueOf((buf ++ Rope.fromArray(now.toArray)).toArray))
//            val nextItr = FlattenI(cur <<: enumInput(curInput))
//            FlattenI(nextItr.fold[Iteratee[CharBuffer, M, Iteratee[String,M,A]]](
//              cont = _ => step(Rope.empty, nextItr)(nextInput).pure,
//              done = (_, _) => Done(nextItr, nextInput).pure,
//              error = (msg, _) => iteratees.Failure(msg, e).pure
//            ))
//          }
//        case e @ EOF(None) =>
//          val lastLine = String.valueOf(buf.toArray)
//          Done(FlattenI(cur <<: enumInput(Chunk(lastLine))),e)   // TODO - Use EmptyChunk and provide one for CharBuffer/ByteBuffer
//        case e @ EOF(Some(err)) => iteratees.Failure(err, e)
//      }
//      Cont(step(Rope.empty, i))
//    }
//  }
//}
