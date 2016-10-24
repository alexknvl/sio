package sio

import sio.core.IO
import scala.io.StdIn

package object teletype {
  /**
    * Read a character from the standard input device.
    */
  val getChar: IO[Char] = IO { StdIn.readChar() }

  /**
    * Read a line from the standard input device
    */
  def getLine: IO[String] = IO { StdIn.readLine() }

  /**
    * Write a character to the standard output device.
    */
  def putChar(c: Char): IO[Unit] = IO { print(c) }

  /**
    * Write a string to the standard output device.
    */
  def putStr(s: String): IO[Unit] = IO { print(s) }

  /**
    * The same as [[putStr]], but adds a newline character.
    */
  def putStrLn(s: String): IO[Unit] = IO { println(s) }
}
