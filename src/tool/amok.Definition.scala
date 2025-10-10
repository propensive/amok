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
