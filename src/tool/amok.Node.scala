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

import scala.collection.mutable as scm
import scala.collection.immutable.ListMap

import soundness.{is as _, Node as _, *}

class Node(parent0: Optional[Node] = Unset):
  private var doc: Optional[Text] = Unset
  val membersMap: scm.TreeMap[Member, Node] = scm.TreeMap()
  var template: Optional[Template] = Unset
  var definition: Optional[Definition] = Unset
  var memo: Optional[InlineMd] = Unset
  var detail: Optional[Text] = Unset
  var hidden: Boolean = false
  var aliases: List[Typename] = Nil

  //def declaration: Declaration = declarations.last
  def declarations: List[Declaration] = List(definition, template).compact
  def parent: Node = parent0.or(this)
  def members: List[(Member, Node)] = membersMap.to(List)

  override def toString: String = members.map(_(0).text).join(t", ").s

  def terms: ListMap[Text, Node] =
    members.collect:
      case (Member.Root(name), node)   => name -> node
      case (Member.OfTerm(name), node) => name -> node

    . to(ListMap)

  def types: ListMap[Text, Node] =
    members.collect { case (Member.OfType(name), node) => name -> node }.to(ListMap)

  def apply(member: Member): Optional[Node] =
    membersMap.get(member).optional

  def update(definition0: Definition): Unit = definition = definition0
  def update(template0: Template): Unit = template = template0

  def update(member: Member): Node =
    membersMap.get(member).getOrElse:
      Node(this).tap: node =>
        membersMap(member) = node

  def tree(name: Text, group: Text, path: Text)(using mountpoint: Mountpoint): Element["details"] =
    import html5.*
    val members2 = members.filter(!_(1).hidden)

    Details(name = group.urlEncode, id = DomId(t"menu_${path}"))
     (if members2.isEmpty
      then Summary(A(href = mountpoint / "_entity" / path, target = id"main")(name))
      else Summary.full(A(href = mountpoint / "_entity" / path, target = id"main")(name)),
      Div:
        members2.map: (member, node) =>
          node.tree(member.text, path, path+member.safe))
