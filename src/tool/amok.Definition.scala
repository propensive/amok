package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

enum Definition:
  case `package`(modifiers: List[Modifier])
  case `object`(modifiers: List[Modifier])
  case `case object`(modifiers: List[Modifier])
  case `enum.case`(modifiers: List[Modifier])
  case `def`(modifiers: List[Modifier])
  case `val`(modifiers: List[Modifier])
  case `var`(modifiers: List[Modifier])
  case `given`(modifiers: List[Modifier])
  case `extension`(params: Syntax, `def`: Definition.`def`, modifiers: List[Modifier] = Nil)

  def keyword: Syntax = Syntax.Symbolic:
    this match
      case _: `package`            => t"package"
      case _: `object`             => t"object"
      case _: `case object`        => t"case object"
      case _: `enum.case`          => t"case"
      case _: `def`                => t"def"
      case _: `val`                => t"val"
      case _: `var`                => t"var"
      case _: `given`              => t"given"
      case _: `extension`          => t"extension"

  def modifiers: List[Modifier]

  def syntax: Syntax = this match
    case `extension`(param, definition, _) =>
      Syntax(0, Syntax.Symbolic(t"extension"), Syntax.Space, param, Syntax.Space, definition.syntax)
    case other =>
      Syntax.sequence(other.modifiers.map(_.keyword), Syntax.Space)
      . let(Syntax(0, _, Syntax.Space, keyword))
      . or(keyword)

export Definition.*
