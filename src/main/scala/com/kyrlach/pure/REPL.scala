package com.kyrlach.pure

import cats.effect.{ExitCode, IO, IOApp}

object REPL extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO.println("Hello, world.").as(ExitCode.Success)
}
