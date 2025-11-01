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

import soundness.{Node as _, *}

object Member:
  def unapply(text: Any): Option[Member] = text match
    case text: Text => Some(Typename(text.sub(t"~", t"#")).member)
    case _          => None

  def urlDecode(typename: Typename): Typename = typename match
    case Typename.Top(name)          => Typename.Top(name.urlDecode)
    case Typename.Type(parent, name) => Typename.Type(urlDecode(parent), name.urlDecode)
    case Typename.Term(parent, name) => Typename.Term(urlDecode(parent), name.urlDecode)

  given decodable: Member is Decodable in Text =
    text => urlDecode(Typename(text.sub("~", "#"))).member

  given encodable: Member is Encodable in Text =
    case Member(Unset, name)                 => name
    case Member(parent: Typename.Term, name) => t"${parent.url}.$name"
    case Member(parent: Typename.Type, name) => t"${parent.url}~$name"
    case Member(parent: Typename.Top, name)  => t"${parent.url}.$name"

case class Member(parent: Optional[Typename], name: Text):
  val text: Text = parent.lay(name) { parent => t"${parent.render}$symbol$name" }
  def template: Typename = parent.let(Typename.Type(_, name)).or(Typename.Top(name))
  def definition: Typename = parent.let(Typename.Term(_, name)).or(Typename.Top(name))

  def symbol: Text = parent match
    case Unset | _: Typename.Top => t"."
    case _: Typename.Term        => t"."
    case _: Typename.Type        => t"⌗"
