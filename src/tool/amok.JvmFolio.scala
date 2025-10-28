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

import Markdown.renderable

class JvmFolio(mountpoint: Mountpoint, source: Path on Linux, var model: Model = Model())
extends Folio(mountpoint, t"jvm", source):

  given Mountpoint = mountpoint
  given Model = model

  def handle(using Http.Request): Http.Response = subpath match
    case _ /: t"api.css"     => Http.Response(cp"/amok/api.css")
    case _ /: t"code.css"    => Http.Response(cp"/amok/code.css")
    case _ /: t"utils.js"    => Http.Response(cp"/amok/utils.js")
    case _ /: t"navigate.js" => Http.Response(cp"/amok/navigate.js")
    case _ /: t"logo.svg"    => Http.Response(cp"/amok/logo.svg")

    case _ /: t"_entity" /: (name: Text) =>
      val (symbol, entity, node) = model.resolve(name)

      Http.Response:
        import html5.*
        recover:
          case MarkdownError(_) | CodlError(_) | ParseError(_, _, _) =>
            Page.simple(mountpoint, H2(t"Error"), P(t"The page contained errors"))

        . within:
            val detail: Optional[Markdown[Markdown.Ast.Block]] =
              node.detail.let(_.read[Md])

            val typename = Typename.decode(name)

            def parents: List[Typename] =
              def recur(typename: Typename): List[Typename] =
                typename.only:
                  case Typename.Type(parent, _) => parent :: recur(parent)
                  case Typename.Term(parent, _) => parent :: recur(parent)
                . or(Nil)

              recur(typename)

            def link(parent: Declaration, name: Text, child: Declaration): Path on Www =
              val isType = parent match
                case parent: Definition => false
                case parent: Template   => true

              (mountpoint / "_entity" / typename.child(name, isType).id).on[Www]

            def keywords(node: Node) =
              List(node.template.let(_.syntax().html), node.definition.let(_.syntax().html)).compact match
                case one :: Nil        => Code(one)
                case one :: two :: Nil => Code(one, t", ", two)
                case _                 => Unset

            given imports: Imports = Imports(Set
                   (typename,
                    Typename.decode(t"scala.Predef"),
                    Typename.decode(t"scala.collection"),
                    Typename.decode(t"scala.collection.immutable"),
                    Typename.decode(t"scala"),
                    Typename.decode(t"prepositional"),
                    Typename.decode(t"java.lang")) ++ parents ++ cookie("imports").let(_.cut(t",").to(Set)).or(Set()).map(Typename.decode(_)))

            val colon = if node.types.isEmpty then Nil else List(Code(t":"))

            val pkg = typename match
              case Typename.Top(_) => true
              case _               => false


            Page.simple
             (mountpoint, typename.only:
                case Typename.Type(parent, _) => H1.pkg(Code({ given Imports(Set()); parent.html }, symbol))
                case Typename.Term(parent, _) => H1.pkg(Code({ given Imports(Set()); parent.html }, symbol)),
              H1(Code(entity)),
              node.memo.let { memo => Div(memo.html) },
              Table.members(List(node.template.let(_ -> node.types), node.definition.let(_ -> node.terms)).compact.flatMap:
                case (declaration, members) =>

                  def colon0 = if members.isEmpty then "" else ":"
                  val titleRow = Tr(Td(colspan = 3)(H2(Code(declaration.syntax().html, " ", entity, colon0))))

                  val methods = members.to(List).groupBy(_(1).declaration.group).flatMap: (group, members) =>
                    val head = group.let { ext => List(Tr(Td.extension(Code("extension ", ext.html)))) }.or(Nil)

                    head ::: members.to(List).flatMap: (name, child) =>
                      val href = link(declaration, name, child.declaration)
                      val kinds = child.declarations.flatMap(_.syntax(true).html :+ Code(", ")).dropRight(1)
                      val meta = child.returnType.let(_.html).let(Code(_))
                      val colon = if meta.present then ":" else ""

                      List
                       (Tr.first(Td.kind(rowspan = 2)(Code(kinds)), Td, Td),
                        Tr(Td(Code(A(href = href)(B(name)), child.params.let(_.html), colon)), Td(rowspan = 2)(meta)),
                        Tr(Td, Td),
                        child.memo.let { memo => Tr(Td, Td(colspan = 2)(memo.html)) })

                  titleRow :: methods.to(List)),
            Div(node.detail.let(_.read[Md].html)))

    case _ /: t"_api" =>
      import html5.*
      val rootLocation: Path on Www = mountpoint / "_entity"

      Http.Response:
        Page
         (mountpoint,
          Nil,
          List
           (H2(t"All Packages"),
            Ul.all
             (model.root.members.filter(!_(1).hidden).map: (member, _) =>
                val link: Path on Www = (mountpoint / "_api" / member.text.skip(1)).on[Www]
                Li(Code(A(href = link)(member.text.skip(1)))))))

    case _ /: t"_api" /: (pkg: Text) =>
      import html5.*

      val rootLocation: Path on Www = mountpoint / "_entity" / pkg

      Http.Response:
        Page
         (mountpoint,
          List
           (Details(Summary(H3(Label(Input.Checkbox(id = id"toggle", value = pkg)), A(target = id"main", href = rootLocation)(pkg)))),
            Div.items:
              model(pkg).members.filter(!_(1).hidden).map: (member, node) =>
                node.tree(member.text, pkg, pkg+member.safe)),
          List(Iframe(id = id"api", name = t"main", src = rootLocation)))

    case _ =>
      Server.at(request.location).let(_.handle(using request)).or:
        Http.Response(NotFound(t"Not found"))
