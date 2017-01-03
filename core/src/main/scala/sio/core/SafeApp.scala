package sio.core

trait SafeApp {
  def run(args: List[String]): IO[Unit]

  def main(args: Array[String]): Impure[Unit] =
    IO.unsafeRun(run(args.toList))
}
