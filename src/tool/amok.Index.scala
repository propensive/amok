                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                     ╭─────────╮ ╭───╮╌────╮╌────╮ ╭─────────╮ │   │ ╭───╮                        ┃
┃                     ╰─────╮   │ │   ╭─╮   ╭─╮   │ │   ╭─╮   │ │   │╌╯   │                        ┃
┃                     ╭─────╯   │ │   │ │   │ │   │ │   │ │   │ │        ╌╯                        ┃
┃                     │   ╭─╮   │ │   │ │   │ │   │ │   │ │   │ │   ╭─╮   │                        ┃
┃                     │   ╰─╯   │ │   │ │   │ │   │ │   ╰─╯   │ │   │ │   │                        ┃
┃                     ╰─────────╯ ╰───╯ ╰───╯ ╰───╯ ╰─────────╯ ╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, prerelease version                                                                      ┃
┃    © Copyright 2023-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://github.com/propensive/amok/                                                       ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package amok

import soundness.*

object Index:
  def apply(text: Text): Index = text.where(_ == '.', bidi = Rtl).lay(Top(text)): position =>
    val next = text.after(position)
    if next.ends(t"package$$") then apply(text.before(position))
    else Entity(apply(text.before(position)), false, text.after(position))

  def decode(text: Text): Index =
    def entity(start: Ordinal, end: Ordinal, isType: Boolean, parent: Optional[Index]): Index =
      val part = text.segment(start ~ end)
      parent.lay(Index.Top(part))(Index.Entity(_, isType, part))

    def recur(position: Ordinal, start: Ordinal, isType: Boolean, index: Optional[Index]): Index =
      text.at(position) match
        case Unset            => entity(start, position - 1, isType, index)
        case char@('.' | ':') =>
          recur(position + 1, position + 1, char == ':', entity(start, position - 1, isType, index))
        case char  =>
          recur(position + 1, start, isType, index)

    recur(Prim, Prim, false, Unset)

  given (imports: Imports) => Index is Renderable:
    import html5.*
    type Result = Phrasing

    def html(index: Index): List[Html[Phrasing]] =
      def recur(index: Index): Html[Phrasing] =
        val ref = index.id
        def link(name: Text) = A(href = % / "entity" / ref)(name)

        index match
          case Top(name) =>
            Span(link(name))

          case Entity(parent, isType, name) =>
            if imports.has(parent) then Span(link(name))
            else Span(recur(parent), if isType then t"⌗" else t".", link(name))

      List(recur(index))

enum Index:
  case Top(name: Text)
  case Entity(parent0: Index, isType: Boolean, name: Text)

  def child(name: Text, isType: Boolean) = Entity(this, isType, name)

  def parent: Index = this match
    case Entity(parent, _, _) => parent
    case Top(_)               => this

  def id: Text = this match
    case Top(name)                   => name
    case Entity(parent, false, name) => t"${parent.id}.${name.urlEncode}"
    case Entity(parent, true, name)  => t"${parent.id}:${name.urlEncode}"

  def text: Text = this match
    case Top(name)                   => name
    case Entity(parent, false, name) => t"${parent.text}.$name"
    case Entity(parent, true, name)  => t"${parent.text}⌗$name"

  def apiLink: Path on Rfc3986 = (% / "api" / text.skip(1)).on[Rfc3986]
  def entityLink: Path on Rfc3986 = (% / "entity" / text.skip(1)).on[Rfc3986]
