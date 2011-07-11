package scalaz.nio
import scalaz._
import scalaz.iteratee._
import java.nio.ByteBuffer
import java.nio.channels.{ByteChannel=>JByteChannel}
import Scalaz._

/** This object holds helper methods to use Iteratee I/O against
* java.nio.Channels.
*/
object Channels {

  /**
   * Construct an enumerator of nio ByteChannel's that uses the given buffer.
   * @param channel  The NIO channel to read from.
   * @param buf The buffer to use when reading from the nio channel.   This will be recycled.
   */
  def enumByteChannel[M[_], A](channel : JByteChannel, buf : ByteBuffer = ByteBuffer.allocate(8192))(implicit m: Monad[M]) = {
    implicit val f: Functor[M] = m.functor
    implicit val b: Bind[M] = m.bind
    def enum(i : IterateeT[ByteBuffer, M, A]) : IterT[ByteBuffer,M,A] = {
      // Helper to read from the channel into the ByteBuffer.
      def readBuf(channel : M[JByteChannel]) : M[(JByteChannel,Input[ByteBuffer])] = channel map { c =>
          buf.rewind()
          if (buf.limit() == buf.capacity()) {
            buf.compact();
            buf.limit(buf.position());
            buf.position(0);
          }
          try {
            if(c.read(buf) >= 0) {
              buf.flip()
              (c, elInput(buf))
            } else {
              // Close channel?
              c.close()
              (c, eofInput)
            }
          } catch {
            case x : java.io.IOException => (c, eofInput)
          }
        }
      /// Helper to drive ByteBuffers through the Iteratee.
      def drive(i : IterateeT[ByteBuffer, M, A], state : M[JByteChannel]) : IterT[ByteBuffer, M, A] =
        state flatMap { channel =>
          if(!channel.isOpen) m.point(i) else {
            i.foldT[IterT[ByteBuffer, M, A]](
              done = (_,_) => m.point(i),
              cont = (k) => readBuf(state) >>= { case (channel, input) =>
                k(input) >>= (km => drive(km, m.point(channel)))   // TODO - This isn't 'scala' tail recursion, maybe we should trampoline?
              }
            )
          }
        }
      drive(i, m.point(channel))
    }
    enumeratorT[ByteBuffer, M, A](enum)
  }
  /** An iteratee that will write to a byte channel */
//  def writeToChannel[M[_]](channel: M[JByteChannel])(implicit m: Monad[M]): IterateeT[ByteBuffer, M, Unit] = {
//    def step[A](channel: M[JByteChannel]): Input[ByteBuffer] => IterateeT[ByteBuffer, M, Unit] = {
//      case EOF(None) =>
//        FlattenI(channel.map(_.close()).map(Done(_, EOF[ByteBuffer](None))))
//      case Chunk(buf) =>
//        FlattenI(channel map { c =>
//          c.write(buf)
//          Cont(x => step(m.pure(c))(x))
//        })
//      case EOF(e@Some(err)) => iteratees.Failure(err, EOF(e))
//    }
//    Cont(i => step(channel)(i))
//  }

  // TODO - enumeratee to convert from ByteBuffer to Char stream...
  // TODO - enumeratee to convert ByteBuffer stream to 'Serialized java object' stream?
}
