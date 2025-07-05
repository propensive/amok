package amok

import scala.tasty.*, inspector.*
import scala.quoted.*
import scala.collection.mutable as scm

import soundness.{is as _, Node as _, *}

class Node():
  val membersMap: scm.TreeMap[Member, Node] = scm.TreeMap()
  private var doc: Optional[Text] = Unset
  var signature: Optional[amok.Signature] = Unset
  var typeKind: Optional[TypeKind] = Unset
  var memo: Optional[Text] = Unset
  var detail: Optional[Text] = Unset
  var hidden: Boolean = false
  var returnType: Optional[Typus] = Unset

  def members: List[(Member, Node)] = membersMap.to(List)

  override def toString: String = members.map(_(0).text).join(t", ").s

  def apply(member: Member): Node = membersMap.get(member).getOrElse:
    Node().tap: node =>
      membersMap(member) = node

  def tree(name: Text, group: Text, path: Text): Element["details"] =
    import html5.*
    val members2 = members.filter(!_(1).hidden)

    Details(name = group.urlEncode, id = DomId(t"menu_${path}"))
      (if members2.isEmpty then Summary(A(href = % / "entity" / path, target = id"main")(name))
      else Summary.full(A(href = % / "entity" / path, target = id"main")(name)),
      Div.content:
        members2.map: (member, node) =>
          node.tree(member.text, path, path+member.safe))
