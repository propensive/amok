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

import Markdown.renderable

class JvmFolio(mountpoint: Mountpoint, source: Path on Linux, var model: Model = Model())
extends Folio(mountpoint, t"jvm", source):

  given Mountpoint = mountpoint
  given Model = model

  def handle(using Http.Request, Stdio): Http.Response = subpath match
    case %                   => Http.Response(NoCache(Page(mountpoint, Nil, List(html5.H2("Welcome")))))
    case % /: t"api.css"     => Http.Response(cp"/amok/api.css")
    case % /: t"code.css"    => Http.Response(cp"/amok/code.css")
    case % /: t"utils.js"    => Http.Response(cp"/amok/utils.js")
    case % /: t"navigate.js" => Http.Response(cp"/amok/navigate.js")
    case % /: t"logo.svg"    => Http.Response(cp"/amok/logo.svg")

    case % /: t"_entity" /: Member(member) =>
      import html5.*
      Out.println(t"Looking up ${member.text}")
      model.lookup(member).let: node =>
        Out.println(t"Found ${member.text}")
        val root = model.root(member).vouch
        given RootPackage(root)
        Http.Response:
          recover:
            case MarkdownError(_) | CodlError(_) | ParseError(_, _, _) =>
              NoCache(Page.simple(mountpoint, H2(t"Error"), P(t"The page contained errors")))

          . within:
              def parents: List[Typename] =
                def recur(typename: Typename): List[Typename] = typename match
                  case Typename.Type(parent, _) => parent :: recur(parent)
                  case Typename.Term(parent, _) => parent :: recur(parent)
                  case Typename.Top(_)          => Nil

                recur(member.definition)

              def link(parent: Declaration, name: Text): Path on Www =
                val child: Member = parent match
                  case parent: Definition => member.definition / name
                  case parent: Template   => member.template / name

                mountpoint / "_entity" / child.encode

              def full[result](lambda: Imports ?=> result): result = lambda(using Imports(Set()))

              val cookieImports = cookie("imports").let(_.cut(t",").to(Set)).or(Set()).map(Typename(_))

              val predef = List(t"scala.Predef",
                                t"scala.collection",
                                t"scala.collection.immutable",
                                t"scala",
                                t"java.lang")
                           . map(_.decode[Member])

              given imports: Imports = Imports(Set
                     (Typename.decode(t"scala.Predef"),
                      Typename.decode(t"scala.collection"),
                      Typename.decode(t"scala.collection.immutable"),
                      Typename.decode(t"scala"),
                      Typename.decode(t"java.lang")) ++ parents ++ cookieImports)

              val parentPackage = H1.pkg(Code(full(parents.prim.let { parent => parent.member.html :+ member.symbol })))

              NoCache:
                Page.simple
                 (mountpoint,
                  parentPackage,
                  H1(Code(member.name)),
                  Div(node.info.let(_.html)),
                  Table.members(node.namespace.flatMap:
                    case (declaration, members) =>
                      def colon0 = if members.isEmpty && declaration.returnType.absent then "" else ": "

                      val typename = declaration match
                        case _: Definition => member.definition
                        case _: Template   => member.template

                      given imports2: Imports = Imports(imports.typenames + typename)
                      val keywords = declaration.syntax().html
                      val parameters = declaration.parameters.let(_.html)
                      val returnType = declaration.returnType.let(_.html)
                      val signature = Code(keywords, " ", typename.name, parameters, colon0, returnType)
                      val subhead = Tr(Td(colspan = 3)(H3(signature)))
                      val methods0 = members.map { member => model.lookup(member).let(member -> _) }
                      val grouped = methods0.compact.groupBy(_(1).definition.let(_.group)).to(List)

                      val entries = grouped.flatMap: (group, members) =>
                        val head = group.lay(Nil): group =>
                          List(Tr(Td(Code("extension ", group.html))))

                        head ::: members.flatMap: (member, child) =>
                          val local = true
                          val href = link(declaration, member.name)
                          val kinds = child.declarations.flatMap(_.syntax(true).html :+ Code(", ")).dropRight(1)
                          val meta = child.declarations.map(_.returnType).prim.let(_.html).let(Code(_))
                          val colon = if meta.present then ":" else ""
                          val params = child.declarations.map(_.parameters).compact.prim.let(html)

                          List
                           (Tr.first(Td.kind(rowspan = 2)(Code(kinds)), Td, Td),
                            Tr(Td(Code(A(href = href)(B(member.name)), params, colon)), Td(rowspan = 2)(meta)),
                            Tr(Td, Td),
                            child.info.let { info => Tr(Td, Td(colspan = 2)(info.html)) })

                      subhead :: entries),
                Div(node.document.dare(_.read[Md]).let(_.html)))
      . or:
          Http.Response(NotFound(t"Not found"))

    case % /: t"_api" =>
      import html5.*

      Http.Response:
        NoCache:
          Page
           (mountpoint,
            Nil,
            List
             (H2(t"All Packages"),
              Ul.all
               (model.packages.map: root =>
                  val link: Path on Www = mountpoint / "_api" / root.encode
                  Li(Code(A(href = link)(root.encode))))))

    case % /: t"_api" /: Member(member) =>
      import html5.*

      model.root(member).let: root =>
        val initial: Path on Www = mountpoint / "_entity" / member.encode
        val link = A(target = id"main", href = initial)(root.encode)

        def menu(member: Member): Element["details"] =
          model.lookup(member).lay(Details("missing")): node =>
            val path: Path on Www = mountpoint / "_entity" / member.encode
            val link = A(href = path, target = id"main")(member.symbol, member.name)
            Details(name = member.encode, id = DomId(t"menu_${member.encode}"))
             (if node.members.isEmpty then Summary(link) else Summary.full(link),
              Div(node.typeMembers.sortBy(_.name).map(menu(_)), node.termMembers.sortBy(_.name).map(menu(_))))

        model.lookup(root).lay(Http.Response(NotFound(t"Not found"))): root =>
          Http.Response:
            NoCache:
              Page
               (mountpoint,
                List
                 (Details(Summary(H3(Label(Input.Checkbox(id = id"root", value = root.encode)), link))),
                  Div.items(root.typeMembers.sortBy(_.name).map(menu(_)), root.termMembers.sortBy(_.name).map(menu(_)))),
                List(Iframe(id = id"api", name = t"main", src = initial)))
      . or:
          Http.Response(NotFound(t"Not found"))

    case _ =>
      Server.at(request.location).let(_.handle(using request)).or:
        Http.Response(NotFound(t"Not found"))
