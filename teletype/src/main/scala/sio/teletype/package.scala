package sio

import sio.core.IO
import scala.io.StdIn

package object teletype {
  val getChar: IO[Char] = IO.capture(StdIn.readChar())
  def putChar(c: Char): IO[Unit] = IO.capture(print(c))
  def putStr(s: String): IO[Unit] = IO.capture(print(s))
  def putStrLn(s: String): IO[Unit] = IO.capture(println(s))
  def readLn: IO[String] = IO.capture(StdIn.readLine())
}
