package scalaz
package effects

import java.nio._
import java.io._
import java.nio.channels._

/** Regional file handles. A handle opened with mode M in region R.
  * A handle is backed by a NIO channel and finalized with a reference-counted finalizer upon exit of a region. */
sealed abstract class Handle[M, R[_]](unsafeChannel: Channel) {
  val close: IO[Unit] = IO(rw => (rw, { unsafeChannel.close }))
}
case class ReadOnlyHandle[R[_]](unsafeReadableChannel: ReadableByteChannel)
  extends Handle[RMode, R](unsafeReadableChannel)
case class WriteOnlyHandle[R[_]](unsafeWritableChannel: WritableByteChannel)
  extends Handle[WMode, R](unsafeWritableChannel)
case class ReadWriteHandle[R[_]](unsafeFileChannel: FileChannel)
  extends Handle[RWMode, R](unsafeFileChannel)

sealed trait ReadModes[M]
sealed trait WriteModes[M]

sealed trait RMode
sealed trait WMode
sealed trait RWMode

sealed trait IOMode[M] {
  def open[R[_]](f: File): IO[Handle[M, R]]
}
case object ReadMode extends IOMode[RMode] {
  def open[R[_]](f: File): IO[Handle[RMode, R]] =
    IO(rw => (rw, ReadOnlyHandle[R](new FileInputStream(f).getChannel)))
}
case object WriteMode extends IOMode[WMode] {
  def open[R[_]](f: File): IO[Handle[WMode, R]] =
    IO(rw => (rw, WriteOnlyHandle[R](new FileOutputStream(f).getChannel)))
}
case object ReadWriteMode extends IOMode[RWMode] {
  def open[R[_]](f: File): IO[Handle[RWMode, R]] = 
    IO(rw => (rw, ReadWriteHandle[R](new RandomAccessFile(f, "rw").getChannel)))
}

trait MkIOMode[M] {
  def mkIOMode: IOMode[M]
}

object MkIOMode {
  implicit val mkReadMode: MkIOMode[RMode] = new MkIOMode[RMode] {
    def mkIOMode = ReadMode
  }
  implicit val mkWriteMode: MkIOMode[WMode] = new MkIOMode[WMode] {
    def mkIOMode = WriteMode
  }
  implicit val mkReadWriteMode: MkIOMode[RWMode] = new MkIOMode[RWMode] {
    def mkIOMode = ReadWriteMode
  }
}

object ReadModes {
  implicit val readModeReads: ReadModes[RMode] = new ReadModes[RMode] {}
  implicit val readWriteModeReads: ReadModes[RWMode] = new ReadModes[RWMode] {}
}

object WriteModes {
  implicit val writeModeWrites: WriteModes[WMode] = new WriteModes[WMode] {}
  implicit val readWriteModeWrites: WriteModes[RWMode] = new WriteModes[RWMode] {}
}

object Handle {
  def wrapInputStream[R[_]](i: InputStream): Handle[RMode, R] =
    ReadOnlyHandle[R](Channels.newChannel(i))
  def wrapOutputStream[R[_]](o: OutputStream): Handle[WMode, R] =
    WriteOnlyHandle[R](Channels.newChannel(o))
}

