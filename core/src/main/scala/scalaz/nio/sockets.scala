//package scalaz.nio
//
//import scalaz._
//import effect.IO
//import concurrent.Promise
//import iteratee._
//import java.nio.ByteBuffer
//import java.nio.channels._
//import Scalaz._
//import java.net.{InetSocketAddress, InetAddress}
//import spi.SelectorProvider
//import collection.JavaConversions._
//
//
//object sockets {
//
//  /**Creates a socket server where connections are farmed out inside promises.   Note:
//   * The select calls are defered using the Promise monad and the implicitly supplied
//   * concurrent strategy.
//   *
//   * TODO(joshuasuereth): Allow accepting connections to happen on multiple threads, or perhaps execute selects
//   * using the strategy passed in.
//   *
//   * @param port  The port to bind the server socket to.
//   * @param s  The threading strategy to use when farming out accepted connections for processing.
//   */
//  def serverSocketChannel(port : Int)(implicit s : concurrent.Strategy) : IO[Enumerator[SocketChannel, Promise]] =
//    // This method captures 'unsafe' IO bits that we can embedd in our IO controller.
//    IO.ioPure.pure(new Enumerator[SocketChannel, Promise] {
//        val selector = Selector.open() // TODO - use SelectorProvider?
//        val selectableChannel: ServerSocketChannel = ServerSocketChannel.open()
//        selectableChannel.configureBlocking(false)
//        val socketAddr = new InetSocketAddress(port)
//        selectableChannel.socket.bind(socketAddr)
//        selectableChannel.register(selector, SelectionKey.OP_ACCEPT)
//        def apply[A](i : Iteratee[SocketChannel,Promise,A])(implicit m : Monad[Promise]) : Promise[Iteratee[SocketChannel,Promise,A]] = {
//          def nextInput : Input[SocketChannel] = getNextValidKey(selector, _.isAcceptable).right.map[SocketChannel]{
//              key =>
//              val sschanel = key.channel.asInstanceOf[ServerSocketChannel]
//              sschanel.accept
//            }.fold(
//              ex => EOF(Some(ex.getMessage)),
//              channel => Chunk(channel)
//            )
//          i.fold[Iteratee[SocketChannel,Promise,A]](
//            done = (_,_) => {selectableChannel.close(); Promise(i)},
//            cont = k => Promise(k(nextInput)).flatMap(apply),
//            error = (_,_) => {selectableChannel.close(); Promise(i)}
//          )
//        }
//      })
//
//  /**
//   * Constructs an enumerator that can drive a stream processor (Iteratee) with the input form a socket channel.
//   * @param channel The channel to read
//   * @param bufSize  The size of buffer to allocate for reading.   TODO - predicting buffer size  strategy
//   * TODO - Generic Buf typeclass!
//   * @param s The strategy used to execute delayed work.
//   */
//  def readSocketChannel(channel : SocketChannel, bufSize : Int = 8*1024)(implicit s : concurrent.Strategy) : IO[Enumerator[ByteBuffer, Promise]] =
//    IO.ioPure.pure(new Enumerator[ByteBuffer, Promise] {
//      val selector = Selector.open()
//      channel.configureBlocking(false)
//      channel.register(selector, SelectionKey.OP_READ)
//      def apply[A](i : Iteratee[ByteBuffer,Promise,A])(implicit m : Monad[Promise]) : Promise[Iteratee[ByteBuffer, Promise, A]] = {
//        def nextInput = getNextValidKey(selector, _.isReadable).right.map[Input[ByteBuffer]] { key =>
//          val buf = ByteBuffer.allocate(bufSize)
//          if (channel.read(buf) >= 0) {
//            buf.flip
//            Chunk(buf)
//          } else EOF(None)
//        }.fold[Input[ByteBuffer]](
//          ex => EOF(Some(ex.getMessage)),
//          identity
//        )
//        i.fold[Iteratee[ByteBuffer,Promise,A]](
//          done = (_,_) => Promise(i),
//          cont = k => Promise(k(nextInput)).flatMap(apply),
//          error = (_,_) => Promise(i)
//        )
//      }
//    })
//
//
//  def writeSocketChannel(channel : SocketChannel, buf : ByteBuffer)  = IO.ioPure.pure(channel.write(buf))
//
//  /** Unsafe helper method:  Calls the selector and looks for the next key with the isValid boolean true.
//   * Note:  This currently removes all keys that *do not* meet the isValid criteria...   This could be bad in the
//   * future...
//   */
//  private def getNextValidKey(selector: Selector, isValid: SelectionKey => Boolean) : Either[Exception, SelectionKey] =
//    try {
//      // TODO - Use a more FP approach here
//      // TODO - Don't block forever!
//      // TODO - Maybe we don't want to remove ready keys because others may own them!
//      if (selector.select > 0) {
//        val readyKeys = selector.selectedKeys()
//        val itr = readyKeys.iterator()
//        while (itr.hasNext()) {
//          val key = itr.next().asInstanceOf[SelectionKey]
//          // TODO - should we only do this if the key is valid?
//          itr.remove()
//          if (isValid(key)) {
//            return Right(key)
//          }
//        }
//      }
//      // If we didn't get a result yet, keep trying
//      getNextValidKey(selector, isValid)
//    } catch {
//      case ex: java.io.IOException => Left(ex)
//    }
//}
