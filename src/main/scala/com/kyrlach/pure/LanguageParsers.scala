package com.kyrlach.pure

import cats.data.NonEmptyList
import cats.parse.Rfc5234.{alpha, digit}
import cats.parse.{Parser, Parser0, Rfc5234}

object LanguageParsers {

  val colon: Parser[Unit] = Parser.char(':')
  val equals: Parser[Unit] = Parser.char('=')
  val minus: Parser[Unit] = Parser.char('-')
  val plus: Parser[Unit] = Parser.char('+')
  val dot: Parser[Unit] = Parser.char('.')
  val lsb: Parser[Unit] = Parser.char('[')
  val rsb: Parser[Unit] = Parser.char(']')

  val ws0: Parser0[Unit] = Rfc5234.wsp.rep0.as(())
  val ws1: Parser[Unit] = Rfc5234.wsp.rep.as(())

  val withNewLines: Parser[Unit] = ws0.with1 *> Parser.char('\n')

  val name: Parser[String] = (alpha ~ (alpha | digit).rep0).map {
    case (first, rest) => (first :: rest)
  }.string

  val typeExpression: Parser[AST.TypeExpression] = for {
    fqn <- name.repSep(dot)
    parameters <- typeExpression.rep.between(lsb, rsb).?
  } yield {
    val plist = parameters.map(_.toList).getOrElse(Nil)
    val result: AST.TypeExpression = if (fqn.tail.isEmpty) {
      AST.TypeExpression.Local(fqn.head, plist)
    } else {
      AST.TypeExpression.Qualified(NonEmptyList.fromListUnsafe(fqn.init), AST.TypeExpression.Local(fqn.last, plist))
    }
    result
  }

  val withType: Parser[(String, AST.TypeExpression)] = for {
    vname <- name
    _ <- ws0
    _ <- colon
    _ <- ws0
    texp <- typeExpression
  } yield (vname, texp)

  val fractionalPart: Parser[String] =
    dot *> digit.rep.string

  val numericLiteral: Parser[AST.Value] = for {
    negation <- minus.?.with1
    whole <- digit.rep.string
    fractional <- fractionalPart.?
  } yield {
    val sign = negation.map(_ => "-").getOrElse("")
    val fp = fractional.map("." + _).getOrElse("")
    AST.Value.Numeric(BigDecimal(s"$sign$whole$fp"))
  }

  val value: Parser[AST.Value] = numericLiteral

  val binaryOperator: Parser[AST.BinaryOperator] =
    minus.as(AST.BinaryOperator.Minus) | plus.as(AST.BinaryOperator.Plus)

  val expression: Parser[AST.Expression] = Parser.recursive[AST.Expression] { recurse =>
    val literalExp: Parser[AST.Expression] = for {
      v <- value
    } yield AST.Expression.Literal(v, None)

    val atomic: Parser[AST.Expression] = literalExp

    val binaryOperationExp: Parser[AST.Expression] = Parser.defer(for {
      left <- atomic
      _ <- ws1
      op <- binaryOperator
      _ <- ws1
      right <- recurse
    } yield AST.Expression.BinaryOperation(op, left, right, None))

    binaryOperationExp.backtrack | atomic
  }

  val function: Parser[Unit] = Parser.string("function")

  val const: Parser[Unit] = Parser.string("const")

  val declareConstant: Parser[AST.Declaration] = for {
    _ <- const
    _ <- ws1
    declared <- withType
    _ <- ws1
    _ <- equals
    _ <- ws1
    value <- expression
  } yield AST.Declaration.Constant(declared._1, declared._2, value)

  val declarations: Parser[AST.Declaration] = declareConstant

  val module: Parser0[AST.Module] = for {
    _     <- Parser.string("module")
    _     <- ws1
    mname <- name
    _     <- withNewLines.rep0
    decs  <- declarations.repSep0(withNewLines.rep)
  } yield AST.Module(mname, Nil, decs)
}
