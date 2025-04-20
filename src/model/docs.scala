/*
    Amok, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package amok

import soundness.*

import language.dynamics

object Identifier:
  def apply(name: Text): Identifier =
    val str: String = name.s

    def recur(start: Int = 0, path: Path = Path.Root): Identifier =
      val dot = str.indexOf('.', start)
      val hash = str.indexOf('#', start)

      if dot == hash then Identifier(path, str.substring(start).nn.show)
      else if dot < hash && dot != -1 || hash == -1 then recur(dot + 1, Path.Term(path, str.substring(start, dot).nn.show))
      else recur(hash + 1, Path.Type(path, str.substring(start, hash).nn.show))

    recur()

  def unapply(path: Path): Option[Identifier] = path match
    case Path.Type(path, id) => Some(Identifier(path, id))
    case Path.Term(path, id) => Some(Identifier(path, id))
    case Path.Root           => None

  def fromUrl(url: Text): Identifier = apply:
    url.mapChars:
      case ':' => '#'
      case '(' => '['
      case ')' => ']'
      case ch  => ch

case class Identifier(path: Path, id: Text):
  def text: Text = path match
    case Path.Term(_, _) => t"${path.text}.$id"
    case Path.Type(_, _) => t"${path.text}#$id"
    case Path.Root       => id

  def asType: Path.Type = Path.Type(path, id)
  def asTerm: Path.Term = Path.Term(path, id)

  lazy val url: Text = text.mapChars:
    case '#' => ':'
    case '[' => '('
    case ']' => ')'
    case ch  => ch

enum Path:
  case Term(path: Path, id: Text)
  case Type(path: Path, id: Text)
  case Root

  lazy val text: Text = this match
    case Root => t"_root_"
    case Identifier(Root, id)            => id
    case Identifier(path@Type(_, _), id) => t"${path.text}#$id"
    case Identifier(path, id)            => t"${path.text}.$id"

  @targetName("child")
  infix def / (id: Text): Identifier = Identifier(this, id)
