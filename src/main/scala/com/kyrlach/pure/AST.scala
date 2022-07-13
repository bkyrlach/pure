package com.kyrlach.pure

import cats.data.NonEmptyList
import com.kyrlach.pure.typechecker._

object AST {
  sealed trait Import

  case class Module(imports: List[Import], declarations: List[Declaration])

  sealed trait Value

  object Value {
    case class Numeric(n: BigDecimal) extends Value
  }

  sealed trait BinaryOperator

  object BinaryOperator {
    case object Plus extends BinaryOperator
    case object Minus extends BinaryOperator
  }

  sealed trait Expression {
    def pureType: Option[Types.PureType]
  }

  object Expression {
    case class Literal(value: Value, pureType: Option[Types.PureType]) extends Expression
    case class BinaryOperation(operator: BinaryOperator, leftHandSide: Expression, rightHandSide: Expression, pureType: Option[Types.PureType]) extends Expression
    case class Application(function: Expression, argument: Expression, pureType: Option[Types.PureType]) extends Expression
  }

  sealed trait TypeExpression

  object TypeExpression {
    case class Local(value: String, parameters: List[TypeExpression]) extends TypeExpression
    case class Qualified(prefix: NonEmptyList[String], typeExpression: Local) extends TypeExpression
  }

  case class Field(name: String, typeExpression: TypeExpression)

  case class Constructor(parameters: NonEmptyList[TypeExpression])

  sealed trait Declaration

  object Declaration {
    case class Constant(name: String, typeExpression: TypeExpression, value: Expression) extends Declaration
    case class Function(name: String, typeExpression: TypeExpression, body: Expression) extends Declaration
    case class Variant(name: String, constructors: List[Constructor]) extends Declaration
    case class Record(name: String, fields: NonEmptyList[Field]) extends Declaration
  }
}
