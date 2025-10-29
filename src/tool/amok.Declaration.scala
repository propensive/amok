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

sealed trait Declaration:
  def syntax(brief: Boolean = false): Syntax
  def keyword: Syntax
  def modifiers: List[Modifier]
  def parameters: Optional[Syntax]
  def returnType: Optional[Syntax]

  def group: Optional[Syntax] = this match
    case `extension`(params, _, _) => params
    case _                         => Unset

enum Definition extends Declaration:
  case `package`(modifiers: List[Modifier])
  case `object`(modifiers: List[Modifier])
  case `enum.case`(modifiers: List[Modifier], parameters0: Optional[Syntax])
  case `def`(modifiers: List[Modifier], parameters0: Optional[Syntax], returnType0: Optional[Syntax])
  case `val`(modifiers: List[Modifier], returnType0: Optional[Syntax])
  case `var`(modifiers: List[Modifier], returnType0: Optional[Syntax])
  case `given`(modifiers: List[Modifier], returnType0: Optional[Syntax])
  case `extension`(params: Syntax, `def`: Definition.`def`, modifiers: List[Modifier] = Nil)

  def keyword: Syntax = Syntax.Symbolic:
    this match
      case _: `package`            => t"package"
      case _: `object`             => t"object"
      case _: `enum.case`          => t"case"
      case _: `def`                => t"def"
      case _: `val`                => t"val"
      case _: `var`                => t"var"
      case _: `given`              => t"given"
      case _: `extension`          => t"extension"

  def parameters: Optional[Syntax] = this match
    case definition: `enum.case` => definition.parameters0
    case definition: `def`       => definition.parameters0
    case _                       => Unset

  def returnType: Optional[Syntax] = this match
    case definition: `def`   => definition.returnType0
    case definition: `val`   => definition.returnType0
    case definition: `var`   => definition.returnType0
    case definition: `given` => definition.returnType0
    case _                   => Unset

  def modifiers: List[Modifier]

  def syntax(brief: Boolean): Syntax = this match
    case `extension`(param, definition, _) =>
      if brief then definition.syntax(false)
      else Syntax.Compound(List(Syntax.Symbolic(t"extension "), param, Syntax.Symbolic(t" "), definition.syntax(false)))

    case other =>
      val modifiers = other.modifiers.flatMap(_.keyword :: Syntax.Symbolic(t" ") :: Nil)
      if modifiers.isEmpty then keyword else Syntax.Compound(modifiers ::: keyword :: Nil)

export Definition.*

enum Template extends Declaration:
  case `case class`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil, parameters: Optional[Syntax])
  case `class`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, parameters: Optional[Syntax], derivations: List[Text] = Nil)
  case `trait`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, parameters: Optional[Syntax])
  case `enum`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, derivations: List[Text] = Nil, parameters: Optional[Syntax])
  case `case`(modifiers: List[Modifier], extensions: List[Syntax] = Nil, parameters: Optional[Syntax])
  case `type`(modifiers: List[Modifier], extensions: Nil.type = Nil, parameters: Optional[Syntax])

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
  def returnType: Optional[Syntax] = Unset

  def syntax(brief: Boolean): Syntax =
    val modifiers2 = modifiers.flatMap(_.keyword :: Syntax.Symbolic(t" ") :: Nil)
    if modifiers2.isEmpty then keyword else Syntax.Compound(modifiers2 ::: keyword :: Nil)
