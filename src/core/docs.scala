/*
    Amok, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import serpentine.*
import gossamer.*
import cellulose.*
import deviation.*

import scala.collection.mutable.HashMap

import language.dynamics


case class NodeInfo(name: Text, icon: Icons.Icon)

object DocNode:
  val root = DocNode(NodeInfo(t"", Icons.Icon(Icons.Entity.Package, List())))
  def update(path: Text, info: NodeInfo): DocNode = update(DocPath.parse(path.s, false), info)
  
  def update(path: DocPath, info: NodeInfo): DocNode =
    unapply(path.init).get.put(path.last, info)

  def unapply(path: DocPath): Option[DocNode] =
    def recur(path: List[DocPathElement], current: DocNode): Option[DocNode] = path match
      case Nil                     => Some(current)
      case `current`(item) :: tail => recur(tail, item)
      case _                       => None
    
    recur(path.address.reverse, root)

class DocNode(val info: NodeInfo):
  val terms: HashMap[Text, DocNode] = HashMap()
  val types: HashMap[Text, DocNode] = HashMap()
  
  var hidden: Maybe[Boolean] = Unset
  var summary: Maybe[Text] = Unset
  var content: Maybe[Text] = Unset

  def unapply(element: DocPathElement): Option[DocNode] = element match
    case DocPathElement.Type(name) => types.get(name)
    case DocPathElement.Term(name) => terms.get(name)

  def put(element: DocPathElement, info: NodeInfo): DocNode = element match
    case DocPathElement.Type(name) => types.getOrElseUpdate(name, DocNode(info))
    case DocPathElement.Term(name) => terms.getOrElseUpdate(name, DocNode(info))

enum DocPathElement:
  case Type(name: Text)
  case Term(name: Text)

object DocPath:
  import DocPathElement.{Term, Type}
  
  def parse(input: String, isType: Boolean = false): DocPath =
    val str = input.map:
      case '(' => '['
      case ')' => ']'
      case ':' => '#'
      case ch  => ch

    val dot = str.lastIndexOf(".")
    val hash = str.lastIndexOf("#")
    val last = str.substring(dot.max(hash) + 1).nn.show
    val suffix = if isType then Type(last) else Term(last)
    
    if dot == hash then DocPath(List(suffix))
    else parse(str.substring(0, dot.max(hash)).nn, dot < hash) / suffix
    
case class DocPath(address: List[DocPathElement]):
  import DocPathElement.{Term, Type}

  def last: DocPathElement = address.head
  def init: DocPath = DocPath(address.tail)

  def /(name: Text) = DocPath(Term(name) :: address)
  def /#(name: Text) = DocPath(Type(name) :: address)

  def /(element: DocPathElement): DocPath = DocPath(element :: address)

  def url: Text = text.mapChars:
    case '#' => ':'
    case '[' => '('
    case ']' => ')'
    case ch  => ch

  def text: Text =
    def recur(address: List[DocPathElement]): Text = address match
      case Nil                => t""
      case Term(head) :: Nil  => head
      case Type(head) :: Nil  => head
      case Term(head) :: tail => t"$head.${recur(tail)}"
      case Type(head) :: tail => t"$head#${recur(tail)}"

    recur(address.reverse)