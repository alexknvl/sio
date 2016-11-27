package sio.core

import sio.core

trait SafeApp {
  def run(args: List[String]): IO[Unit]

  def main(args: Array[String]): Unit =
    IO.unsafeRun(run(args.toList))
}
