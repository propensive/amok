package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

object Member:
  given ordering: Ordering[Member] = Ordering.by[Member, String]:
    case Member.Root(left)   => left.s
    case Member.OfType(left) => left.s
    case Member.OfTerm(left) => "."+left.s

enum Member:
  case OfTerm(name: Text)
  case OfType(name: Text)
  case Root(name: Text)

  def text: Text = this match
    case OfTerm(name) => t".$name"
    case OfType(name) => t"⌗$name"
    case Root(name)   => name

  def safe: Text = this match
    case OfTerm(name) => t".${name.urlEncode}"
    case OfType(name) => t":${name.urlEncode}"
    case Root(name)   => name
