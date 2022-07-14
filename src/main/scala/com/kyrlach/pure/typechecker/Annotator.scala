package com.kyrlach.pure.typechecker

import cats.syntax.all._
import com.kyrlach.pure.AST
import com.kyrlach.pure.AST.{BinaryOperator, Declaration, Expression, Value}
import com.kyrlach.pure.typechecker.TypeChecker.{CheckProgram, pure}

object Annotator {
  def annotate(module: AST.Module): CheckProgram[AST.Module] =
    module.declarations.traverse(annotate).map(ads => module.copy(declarations = ads))

  def annotate(declaration: AST.Declaration): CheckProgram[AST.Declaration] = declaration match {
    case dc: Declaration.Constant => annotate(dc.value).map(av => dc.copy(value = av))
    case df: Declaration.Function => ???
    case dv: Declaration.Variant  => ???
    case dr: Declaration.Record   => ???
  }

  def annotate(expression: AST.Expression): CheckProgram[AST.Expression] = expression match {
    case literal: Expression.Literal       => literal.value match {
      case Value.Numeric(_) => pure(literal.copy(pureType = Some(Types.Numeric)))
    }
    case binop: Expression.BinaryOperation => for {
      al <- annotate(binop.leftHandSide)
      ar <- annotate(binop.rightHandSide)
      ot =  binop.operator match {
        case BinaryOperator.Plus | BinaryOperator.Minus => Types.Numeric
      }
    } yield binop.copy(leftHandSide = al, rightHandSide = ar, pureType = Some(ot))
    case app: Expression.Application       => ???
  }
}
