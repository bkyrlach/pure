package com.kyrlach.pure.typechecker

import cats.data.StateT
import com.kyrlach.pure.AST

object TypeChecker {
  case class TypeEnv()

  type CheckResult[A] = Either[String, A]
  type CheckProgram[A] = StateT[CheckResult,TypeEnv,A]

  def pure[A](value: A): CheckProgram[A] = StateT.pure(value)
  def error[A](message: String): CheckProgram[A] = StateT.liftF(Left(message))
  val sat: CheckProgram[Unit] = StateT.pure(())

  def check(module: AST.Module): CheckProgram[AST.Module] = for {
    am <- Annotator.annotate(module)
    um <- Unifier.unify(am)
  } yield um
}
