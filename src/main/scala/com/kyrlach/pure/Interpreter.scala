package com.kyrlach.pure

import cats.syntax.all._

import cats.data.EitherT
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.kyrlach.pure.typechecker.TypeChecker

import scala.io.Source

object Interpreter extends IOApp {

  type InterpreterProgram[A] = EitherT[IO,Throwable,A]

  def pure[A](value: A): InterpreterProgram[A] = EitherT.pure(value)
  def liftF[A](effect: IO[A]): InterpreterProgram[A] = EitherT.liftF(effect)
  def makeSafe[A](effect: IO[A]): InterpreterProgram[A] = EitherT(effect.attempt)

  def openFile(path: String): Resource[InterpreterProgram,Source] = Resource.make(makeSafe(IO(Source.fromFile(path))))(s => makeSafe(IO(s.close())))

  override def run(args: List[String]): IO[ExitCode] =
    openFile(args.head).use(s => parseAndEval(s)).value >>= {
      case Left(err) => IO.println(err) *> IO.pure(ExitCode.Error)
      case Right(_)  => IO.pure(ExitCode.Success)
    }

  def parseAndEval(source: Source): InterpreterProgram[Unit] = for {
    code          <- liftF(IO(source.getLines().toList.mkString("\n")))
    module        <- LanguageParsers.module.parseAll(code) match {
      case Left(err) => makeSafe(IO(throw new Exception(err.toString)))
      case Right(m)  => pure(m)
    }
    checkedModule <- TypeChecker.check(module).runA(TypeChecker.TypeEnv()) match {
      case Left(err) => makeSafe(IO(throw new Exception(err.toString)))
      case Right(m)  => pure(m)
    }
  } yield ()
}
