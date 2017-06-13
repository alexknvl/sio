package sio

import sio.core._
import scala.io.StdIn

package object teletype {
  /**
    * Read a character from the standard input device.
    */
  val getChar: ST[RW, Char] = IO { StdIn.readChar() }

  /**
    * Read a line from the standard input device
    */
  def getLine: ST[RW, String] = IO { StdIn.readLine() }

  /**
    * Write a character to the standard output device.
    */
  def putChar(c: Char): ST[RW, Unit] = IO { print(c) }

  /**
    * Write a string to the standard output device.
    */
  def putStr(s: String): ST[RW, Unit] = IO { print(s) }

  /**
    * The same as [[putStr]], but adds a newline character.
    */
  def putStrLn(s: String): ST[RW, Unit] = IO { println(s) }
}
