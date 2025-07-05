package amok

import soundness.*

object Address:
  def decode(text: Text): Address =
    import Address.{Top, Entity}

    def entity(start: Ordinal, end: Ordinal, isType: Boolean, parent: Optional[Address]): Address =
      val part = text.segment(start ~ end)
      parent.lay(Address.Top(part))(Address.Entity(_, isType, part))

    def recur(index: Ordinal, start: Ordinal, isType: Boolean, address: Optional[Address]): Address =
      text.at(index) match
        case Unset            => entity(start, index - 1, isType, address)
        case char@('.' | ':') =>
          recur(index + 1, index + 1, char == ':', entity(start, index - 1, isType, address))
        case char  =>
          recur(index + 1, start, isType, address)

    recur(Prim, Prim, false, Unset)

  given (imports: Imports) => Address is Renderable:
    import html5.*
    type Result = Phrasing

    def html(address: Address): List[Html[Phrasing]] =
      def recur(address: Address, content: List[Html[Phrasing]]): Html[Phrasing] =
        val ref = address.id
        def span(name: Text) = Span(A(href = % / "entity" / ref)(name), content)

        address match
          case Top(name)                    => span(name)

          case Entity(parent, isType, name) =>
            if imports.addresses.contains(parent) then span(name) else
              val link = A(href = % / "entity" / ref)(name)
              recur(parent, (if isType then t"#" else t".") :: link :: content)

      List(recur(address, Nil))

case class Imports(addresses: Set[Address])

enum Address:
  case Top(name: Text)
  case Entity(parent: Address, isType: Boolean, name: Text)

  def id: Text = this match
    case Top(name)                   => name
    case Entity(parent, false, name) => t"${parent.id}.${name.urlEncode}"
    case Entity(parent, true, name)  => t"${parent.id}:${name.urlEncode}"

  def text: Text = this match
    case Top(name)                   => name
    case Entity(parent, false, name) => t"${parent.text}.$name"
    case Entity(parent, true, name)  => t"${parent.text}#$name"
