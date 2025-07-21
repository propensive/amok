package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

enum Template:
  case `case class`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil)
  case `class`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil)
  case `trait`(modifiers: List[Modifier], extensions: List[Syntax] = Nil)
  case `enum`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil)
  case `case`(modifiers: List[Modifier], extensions: List[Syntax] = Nil)
  case `type`(modifiers: List[Modifier], extensions: Nil.type = Nil)

  def extensions: List[Syntax]

  def keyword: Syntax.Symbolic = Syntax.Symbolic:
    this match
      case _: `case class` => t"case class"
      case _: `class`      => t"class"
      case _: `trait`      => t"trait"
      case _: `enum`       => t"enum"
      case _: `case`       => t"case"
      case _: `type`       => t"type"

  def modifiers: List[Modifier]

  def syntax: Syntax =
    Syntax.sequence(modifiers.map(_.keyword), Syntax.Space).let(Syntax(0, _, Syntax.Space, keyword))
    . or(keyword)

export Template.*
