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
    case %                   => Http.Response(Page(mountpoint, Nil, List(html5.H2("Welcome"))))
    case % /: t"api.css"     => Http.Response(cp"/amok/api.css")
    case % /: t"code.css"    => Http.Response(cp"/amok/code.css")
    case % /: t"utils.js"    => Http.Response(cp"/amok/utils.js")
    case % /: t"navigate.js" => Http.Response(cp"/amok/navigate.js")
    case % /: t"logo.svg"    => Http.Response(cp"/amok/logo.svg")

    case % /: t"_entity" /: (name: Text) =>
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

            def link(parent: Declaration, name: Text, local: Boolean): Path on Www =
                val isType = parent match
                  case parent: Definition => false
                  case parent: Template   => true

                if local
                then mountpoint / "_entity" / typename.child(name, isType).id
                else mountpoint / "_api" / typename.child(name, isType).id

            def full[result](lambda: Imports ?=> result): result = lambda(using Imports(Set()))

            val cookieImports = cookie("imports").let(_.cut(t",").to(Set)).or(Set()).map(Typename.decode(_))

            given imports: Imports = Imports(Set
                   (Typename.decode(t"scala.Predef"),
                    Typename.decode(t"scala.collection"),
                    Typename.decode(t"scala.collection.immutable"),
                    Typename.decode(t"scala"),
                    Typename.decode(t"prepositional"),
                    Typename.decode(t"java.lang")) ++ parents ++ cookieImports)

            val parentPackage = H1.pkg(Code(full(typename.parent.html), symbol))

            Page.simple
             (mountpoint,
              parentPackage,
              H1(Code(entity)),
              Div(node.memo.let { memo => memo.html }),
              Table.members(List(node.template.let(_ -> node.types), node.definition.let(_ -> node.terms)).compact.flatMap:
                case (declaration, members) =>
                  def colon0 = if members.isEmpty && declaration.returnType.absent then "" else ": "
                  val keywords = declaration.syntax().html
                  val parameters = declaration.parameters.let(_.html)
                  val returnType = declaration.returnType.let(_.html)
                  val title = H3(Code(keywords, " ", entity, parameters, colon0, returnType))
                  val titleRow = Tr(Td(colspan = 3)(title))
                  val current = declaration match
                    case _: Definition => Typename.Term(typename, entity)
                    case _: Template   => Typename.Type(typename, entity)

                  val methods = members.groupBy(_(1).definition.let(_.group)).flatMap: (group, members) =>
                    val head = group.lay(Nil) { ext => List(Tr(Td(Code("extension ", ext.html)))) }

                    head ::: members.to(List).flatMap: (name, child) =>
                      given imports2: Imports = Imports(imports.typenames + current)
                      val local = true
                      val href = link(declaration, name, local)
                      val kinds = child.declarations.flatMap(_.syntax(true).html :+ Code(", ")).dropRight(1)
                      val meta = child.declarations.map(_.returnType).prim.let(_.html).let(Code(_))
                      val colon = if meta.present then ":" else ""
                      val params = child.declarations.map(_.parameters).compact.prim.let(html)

                      List
                       (Tr.first(Td.kind(rowspan = 2)(Code(kinds)), Td, Td),
                        Tr(Td(Code(A(href = href)(B(name)), params, colon)), Td(rowspan = 2)(meta)),
                        Tr(Td, Td),
                        child.memo.let { memo => Tr(Td, Td(colspan = 2)(memo.html)) })

                  titleRow :: methods.to(List)),
            Div(node.detail.let(_.read[Md].html)))

    case % /: t"_api" =>
      import html5.*
      Http.Response:
        Page
         (mountpoint,
          Nil,
          List
           (H2(t"All Packages"),
            Ul.all
             (model.root.members.filter(!_(1).hidden).map: (member, _) =>
                val link: Path on Www = mountpoint / "_api" / member.text.skip(1)
                Li(Code(A(href = link)(member.text.skip(1)))))))

    case % /: t"_api" /: (pkg: Text) =>
      import html5.*

      val rootLocation: Path on Www = mountpoint / "_entity" / pkg
      val link = A(target = id"main", href = rootLocation)(pkg)

      Http.Response:
        Page
         (mountpoint,
          List
           (Details(Summary(H3(Label(Input.Checkbox(id = id"toggle", value = pkg)), link))),
            Div.items:
              model(pkg).members.filter(!_(1).hidden).map: (member, node) =>
                node.tree(member.text, pkg, pkg+member.safe)),
          List(Iframe(id = id"api", name = t"main", src = rootLocation)))

    case _ =>
      Http.Response(subpath.inspect)
      // Server.at(request.location).let(_.handle(using request)).or:
      //   Http.Response(NotFound(t"Not found"))
