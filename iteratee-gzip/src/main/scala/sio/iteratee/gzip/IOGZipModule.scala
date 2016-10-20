package sio.iteratee.gzip

import java.io._
import java.util.zip.{GZIPOutputStream, GZIPInputStream}

import cats.MonadError
import io.iteratee.internal.Step
import io.iteratee.{Iteratee, Module, Enumerator}
import sio.core.IO
import sio.iteratee.IOModule

trait GZipModule[F[_]] {
  def readLines(file: File): Enumerator[F, String]
  def readLinesFromStream(stream: InputStream): Enumerator[F, String]
  def readBytes(file: File): Enumerator[F, Array[Byte]]
  def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]]

  def writeLines(file: File): Iteratee[F, String, Unit]
  def writeLinesToStream(stream: OutputStream): Iteratee[F, String, Unit]
  def writeBytes(file: File): Iteratee[F, Array[Byte], Unit]
  def writeBytesToStream(stream: OutputStream): Iteratee[F, Array[Byte], Unit]
}

trait SuspendableGZipModule[F[_]] extends GZipModule[F] {
  this: Module[F] { type M[f[_]] <: MonadError[f, Throwable] } =>

  protected def captureEffect[A](a: => A): F[A]

  protected def close(c: Closeable): F[Unit] =
    captureEffect { c.close() }

  protected final def bracket[R <: Closeable, A](fr: F[R])(f: R => F[A]): F[A] = {
    F.flatMap(fr) { r =>
      F.handleErrorWith(f(r)) { e =>
        F.flatMap(F.handleError(close(r))(_ => ()))(_ => F.raiseError(e))
      }
    }
  }

  private[this] def newFileInputStream(file: File): F[FileInputStream] =
    captureEffect { new FileInputStream(file) }
  private[this] def newFileOutputStream(file: File): F[FileOutputStream] =
    captureEffect(new FileOutputStream(file))

  private[this] def newGZIPBufferedReader(stream: InputStream): F[BufferedReader] =
    captureEffect { new BufferedReader(new InputStreamReader(new GZIPInputStream(stream))) }
  private[this] def newGZIPBufferedWriter(stream: OutputStream): F[BufferedWriter] =
    captureEffect { new BufferedWriter(new OutputStreamWriter(new GZIPOutputStream(stream))) }

  private[this] def newGZIPBufferedInputStream(stream: InputStream): F[BufferedInputStream] =
    captureEffect { new BufferedInputStream(new GZIPInputStream(stream)) }
  private[this] def newGZIPBufferedOutputStream(stream: OutputStream): F[BufferedOutputStream] =
    captureEffect { new BufferedOutputStream(new GZIPOutputStream(stream)) }

  private[this] final class LineEnumerator(reader: BufferedReader) extends Enumerator[F, String] {
    def apply[A](s: Step[F, String, A]): F[Step[F, String, A]] =
      if (s.isDone) F.pure(s) else F.flatMap(captureEffect(reader.readLine())) {
        case null => F.pure(s)
        case line => F.flatMap(s.feedEl(line))(apply)
      }
  }

  private[this] final class ByteEnumerator(stream: InputStream, bufferSize: Int = 8192)
    extends Enumerator[F, Array[Byte]] {
    def apply[A](s: Step[F, Array[Byte], A]): F[Step[F, Array[Byte], A]] =
      if (s.isDone) F.pure(s) else F.flatten(
        captureEffect {
          val array = new Array[Byte](bufferSize)
          val bytesRead = stream.read(array, 0, bufferSize)
          val read = if (bytesRead == bufferSize) array else array.slice(0, bytesRead)

          if (bytesRead == -1) F.pure(s) else F.flatMap(s.feedEl(read))(apply)
        }
      )
  }

  final def readLines(file: File): Enumerator[F, String] =
    Enumerator.liftM(bracket(newFileInputStream(file))(newGZIPBufferedReader))(F)
      .flatMap(reader => new LineEnumerator(reader).ensure(close(reader))(F))(F)

  final def readLinesFromStream(stream: InputStream): Enumerator[F, String] =
    Enumerator.liftM(newGZIPBufferedReader(stream))(F)
      .flatMap(reader => new LineEnumerator(reader).ensure(close(reader))(F))(F)

  final def readBytes(file: File): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(bracket(newFileInputStream(file))(newGZIPBufferedInputStream))(F)
      .flatMap(stream => new ByteEnumerator(stream).ensure(close(stream))(F))(F)

  final def readBytesFromStream(stream: InputStream): Enumerator[F, Array[Byte]] =
    Enumerator.liftM(newGZIPBufferedInputStream(stream))(F)
      .flatMap(stream => new ByteEnumerator(stream).ensure(close(stream))(F))(F)

  final def writeLines(file: File): Iteratee[F, String, Unit] =
    Iteratee.liftM(bracket(newFileOutputStream(file))(newGZIPBufferedWriter))(F)
      .flatMap { writer =>
        Iteratee.foldM[F, String, Unit](())((_, line) =>
          captureEffect {
            writer.write(line)
            writer.newLine()
          }
        )(F).ensure(close(writer))(F)
      }(F)

  final def writeLinesToStream(stream: OutputStream): Iteratee[F, String, Unit] =
    Iteratee.liftM(newGZIPBufferedWriter(stream))(F)
      .flatMap { writer =>
        Iteratee.foldM[F, String, Unit](())((_, line) =>
          captureEffect {
            writer.write(line)
            writer.newLine()
          }
        )(F).ensure(close(writer))(F)
      }(F)

  def writeBytes(file: File): Iteratee[F, Array[Byte], Unit] =
    Iteratee.liftM(bracket(newFileOutputStream(file))(newGZIPBufferedOutputStream))(F)
      .flatMap { stream =>
        Iteratee.foldM[F, Array[Byte], Unit](())((_, bytes) =>
          captureEffect(stream.write(bytes))
        )(F).ensure(close(stream))(F)
      }(F)

  def writeBytesToStream(stream: OutputStream): Iteratee[F, Array[Byte], Unit] =
    Iteratee.liftM(newGZIPBufferedOutputStream(stream))(F)
      .flatMap { stream =>
        Iteratee.foldM[F, Array[Byte], Unit](())((_, bytes) =>
          captureEffect(stream.write(bytes))
        )(F).ensure(close(stream))(F)
      }(F)
}

trait IOGZipModule extends IOModule with SuspendableGZipModule[IO]

object `package` extends IOGZipModule