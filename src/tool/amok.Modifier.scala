package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

object Modifier:
  val all: List[Modifier] = values.to(List)

enum Modifier:
  case `private`, `abstract`, `open`, `final`, `erased`, `transparent`, `inline`, `lazy`, `sealed`,
      `override`, `opaque`, `infix`, `into`, `tracked`, `protected`

  def keyword: Syntax = Syntax.Symbolic(this.toString.tt.lower)

export Modifier.*
