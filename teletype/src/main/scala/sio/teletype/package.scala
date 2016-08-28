package sio

import sio.core.IO
import scala.io.StdIn

package object teletype {
  val getChar: IO[Char] = IO { StdIn.readChar() }
  def putChar(c: Char): IO[Unit] = IO { print(c) }
  def putStr(s: String): IO[Unit] = IO { print(s) }
  def putStrLn(s: String): IO[Unit] = IO { println(s) }
  def readLn: IO[String] = IO { StdIn.readLine() }
}
