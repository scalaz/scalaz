package scalaz
package effects

/** Regional file handles. A handle opened with mode M in region R.
  * A handle is backed by a NIO channel and finalized with a reference-counted finalizer upon exit of a region. */
sealed abstract class Handle[M, R[_]](unsafeChannel: Channel) {
  val close: IO[Unit] = IO(rw => (rw, { unsafeChannel.close }))
}
case class ReadOnlyHandle[R[_]](unsafeReadableChannel: ReadableByteChannel)
  extends Handle[ReadMode, R](unsafeReadableChannel, finalizer)
case class WriteOnlyHandle[R[_]](unsafeWritableChannel: WritableByteChannel)
  extends Handle[WriteMode, R](unsafeChannel, finalizer)
case class ReadWriteHandle[R[_]](unsafeFileChannel: FileChannel)
  extends Handle[ReadWriteMode, R](fileChannel, finalizer)

sealed trait ReadModes[M]
sealed trait WriteModes[M]

sealed trait IOMode[M] {
  def open(f: File)
}
case object ReadMode extends IOMode[ReadMode] {
  def open(f: File): IO[Handle[ReadMode, R]] =
    IO(rw => (rw, new ReadOnlyHandle(f.getChannel)))
}
case object WriteMode extends IOMode[WriteMode]
case object ReadWriteMode extends IOMode[ReadWriteMode]

type Chan[ReadMode] = ReadableByteChannel

trait MkIOMode[M] {
  def mkIOMode: IOMode[M]
}

object MkIOMode {
  implicit val mkReadMode: MkIOMode[ReadMode] = new MkIOMode[ReadMode] {
    def mkIOMode = ReadMode
  }
  implicit val mkWriteMode: MkIOMode[WriteMode] = new MkIOMode[WriteMode] {
    def mkIOMode = WriteMode
  }
  implicit val mkReadMode: MkIOMode[ReadWriteMode] = new MkIOMode[ReadWriteMode] {
    def mkIOMode = ReadWriteMode
  }
}

object ReadModes {
  implicit val readModeReads: ReadModes[ReadMode] = new ReadModes[ReadMode] {}
  implicit val readWriteModeReads: ReadModes[ReadWriteMode] = new ReadModes[ReadWriteMode] {}
}

object WriteModes {
  implicit val writeModeWrites: WriteModes[WriteMode] = new WriteModes[WriteMode] {}
  implicit val readWriteModeWrites: WriteModes[ReadWriteMode] = new WriteModes[ReadWriteMode] {}
}

object Handle {
  def handleFromInputStream(i: InputStream) = new Handle[ReadMode] {
    type C = ReadableByteChannel
    private val channel = newChannel(i)
  }
  def handleFromOutputStream(o: OutputStream) = new Handle[WriteMode] {
    type C = WritableByteChannel
    private val channel = newChannel(o)
  }
}

