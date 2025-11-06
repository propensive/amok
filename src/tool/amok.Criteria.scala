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

object Criteria:
  given decodable: Criteria is Decodable in Text = text =>
    import Criterion.*
    def typename(index: Ordinal, start: Ordinal, stack: List[Char]): Text =
      text.at(index) match
        case Unset                                        => text.segment(start till index)
        case char if !stack.isEmpty && char == stack.head => typename(index + 1, start, stack.tail)
        case ' ' if stack.isEmpty                         => text.segment(start till index)

        case '('  => typename(index + 1, start, ')' :: stack)
        case '['  => typename(index + 1, start, ']' :: stack)
        case '"'  => typename(index + 1, start, '"' :: stack)
        case char => typename(index + 1, start, stack)

    def name(index: Ordinal, start: Ordinal, symbol: Optional[Boolean], dotted: Boolean): Text =
      text.at(index) match
        case Unset                 => text.segment(start till index)
        case ' '                   => text.segment(start till index)
        case '_'                   => name(index + 1, start, Unset, dotted)
        case '.' if dotted         => name(index + 1, start, false, dotted)

        case (char: Char) if !char.isLetterOrDigit =>
          if symbol.or(true) then name(index + 1, start, symbol, dotted)
          else text.segment(start till index)

        case char =>
          if symbol.or(false) then text.segment(start till index)
          else name(index + 1, start, symbol, dotted)

    def recur(index: Ordinal, criteria: List[Criterion]): Criteria = text.at(index) match
      case Unset      => Criteria(criteria.reverse*)
      case '!'        => val text = name(index + 1, index + 1, false, true)
                         recur(index + text.length + 1, Package(text) :: criteria)
      case ':'        => recur(index + 1, Type :: criteria)
      case '.'        => recur(index + 1, Term :: criteria)
      case '?'        => recur(index + 1, Given :: criteria)
      case '~'        => recur(index + 1, Extension :: criteria)
      case '~'        => recur(index + 1, Extension :: criteria)
      case '+'        => val text = typename(index + 1, index + 1, Nil)
                         recur(index + text.length + 1, ReturnType(text) :: criteria)
      case '-'        => val text = typename(index + 1, index + 1, Nil)
                         recur(index + text.length + 1, Parameter(text) :: criteria)
      case ' '        => recur(index + 1, criteria)
      case char: Char => val text = name(index, index, !char.isLetterOrDigit, false)
                         recur(index + text.length + 1, Name(text) :: criteria)

    recur(Prim, Nil)


case class Criteria(criterion: Criterion*)

enum Criterion:
  case ReturnType(text: Text)
  case Parameter(text: Text)
  case Name(name: Text)
  case Package(name: Text)
  case Term
  case Type
  case Given
  case Extension
