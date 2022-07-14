package com.kyrlach.pure.typechecker

import cats.syntax.all._
import com.kyrlach.pure.AST
import com.kyrlach.pure.AST.{Declaration, Expression}
import com.kyrlach.pure.typechecker.TypeChecker.{CheckProgram, error, pure, sat}

object Unifier {
  def unify(module: AST.Module): CheckProgram[AST.Module] =
    module.declarations.traverse(unify).map(uds => module.copy(declarations = uds))

  def unify(declaration: AST.Declaration): CheckProgram[AST.Declaration] = declaration match {
    case constant: Declaration.Constant =>
      unify(constant.value).map(uv => constant.copy(value = uv))
    case Declaration.Function(name, typeExpression, body) => ???
    case Declaration.Variant(name, constructors) => ???
    case Declaration.Record(name, fields) => ???
  }

  def getTy(expression: Expression): CheckProgram[Types.PureType] = expression.pureType match {
    case Some(value) => pure(value)
    case None        => error[Types.PureType](s"Expression $expression isn't annotated with a type!")
  }

  def unify(expression: Expression): CheckProgram[AST.Expression] = expression match {
    case literal: Expression.Literal       => pure(literal)
    case binop: Expression.BinaryOperation => for {
      ul <- unify(binop.leftHandSide)
      lt <- getTy(ul)
      rl <- unify(binop.rightHandSide)
      rt <- getTy(rl)
      ot <- getTy(binop)
      _  <- unify(lt, rt)
      _  <- unify(lt, ot)
    } yield binop.copy(leftHandSide = ul, rightHandSide = rl)
    case Expression.Application(function, argument, pureType) => ???
  }

  def unify(ty1: Types.PureType, ty2: Types.PureType): CheckProgram[Unit] = (ty1, ty2) match {
    case (t1, t2) if t1 == t2 => sat
  }
}
