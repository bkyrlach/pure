package com.kyrlach.pure.typechecker

object Types {
  sealed trait PureType

  case object Unit extends PureType

  case object Numeric extends PureType
  case object Char extends PureType
  case object Boolean extends PureType

  case class Arrow(input: PureType, output: PureType) extends PureType
}
