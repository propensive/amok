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

class JvmFolio(mountpoint: Mountpoint, source: Path on Linux, var model: Model = Model())
extends Folio(mountpoint, t"jvm", source):

  given Mountpoint = mountpoint

  def handle(using Http.Request): Http.Response = subpath match
    case _ /: t"api.css"     => unsafely(Http.Response(Classpath / "amok" / "api.css"))
    case _ /: t"code.css"    => unsafely(Http.Response(Classpath / "amok" / "code.css"))
    case _ /: t"utils.js"    => unsafely(Http.Response(Classpath / "amok" / "utils.js"))
    case _ /: t"navigate.js" => unsafely(Http.Response(Classpath / "amok" / "navigate.js"))
    case _ /: t"logo.svg"    => unsafely(Http.Response(Classpath / "amok" / "logo.svg"))

    case _ /: t"_entity" /: (name: Text) =>
      val (symbol, entity, node) = model.resolve(name)

      Http.Response:
        import html5.*
        recover:
          case MarkdownError(_) | CodlError(_) | ParseError(_, _, _) =>
            Page.simple(mountpoint, H2(t"Error"), P(t"The page contained errors"))

        . within:
            import Markdown.renderable

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

            def link(isType: Boolean): Path on Www = (mountpoint / "_entity" / typename.child(name, isType).id).on[Www]
            def keywords(node: Node) =
              List(node.template.let(_.syntax.html), node.definition.let(_.syntax.html)).compact match
                case one :: Nil        => Code(one)
                case one :: two :: Nil => Code(one, t", ", two)
                case _                 => Unset

            given imports: Imports = Imports(Set
                   (Typename.decode(t"scala.Predef"),
                    Typename.decode(t"scala.collection"),
                    Typename.decode(t"scala.collection.immutable"),
                    Typename.decode(t"scala"),
                    Typename.decode(t"prepositional"),
                    Typename.decode(t"java.lang")) ++ parents)

            val colon = if node.types.isEmpty then Nil else List(Code(t":"))

            Page.simple
             (mountpoint, typename.only:
                case Typename.Type(parent, entity) =>
                  H1.pkg(Code(parent.html, symbol))
                case Typename.Term(parent, entity) =>
                  H1.pkg(Code(parent.html, symbol)),
              H1(Code(entity)),
              Div(List(node.template.let(_.syntax), node.definition.let(_.syntax)).compact.map: kind =>
                H3(Code(kind.html))),
              // node.template.let: template =>
              //   val extensionss =
              //     if template.extensions.length == 0 then Unset
              //     else Syntax.sequence(template.extensions, Syntax.Comma).let(_.html)
            //
            //   node.template.let: kind =>
            //     val exts =
            //       if kind.extensions.length == 0 then Unset
            //       else Syntax.sequence(kind.extensions, Syntax.Comma).let(_.html)


            //     Table.members
            //      (Tr(Th(colspan = 2)(Code(kind.syntax.html), Code(t" "), Code(entity, t" extends ".unless(kind.extensions.isEmpty), exts), colon)),
            //       node.types.to(List).flatMap: (name, template) =>
            //         val entry: Html["tr"] = Tr(Td.kind(keywords(template)), Th(Code(A(href = link(true))(name))))

            //         List
            //          (entry,
            //           template.memo.let { memo => Tr(Td, Td.memo(memo.html)) }).compact),

            //   node.definition.let: kind =>
            //     val colon = if node.terms.isEmpty then Nil else List(Code(t":"))

            //     Table.members
            //      (Tr
            //        (Th(colspan = 2)
            //          (Code(kind.syntax.html, t" "),
            //           Code.typed
            //            (entity,
            //             node.params.let(_.html),
            //             t": ".unless(node.returnType.absent),
            //             node.returnType.let(_.html),
            //             colon))),

            //       locally:
            //         val grouped: Seq[Html["tr"]] =
            //           node.terms.to(List).flatMap: (name, node) =>
            //             node.definition match
            //               case `extension`(subject, definition, modifiers) =>
            //                 List((subject, definition.syntax, name, node.memo))
            //               case _ =>
            //                 Nil
            //           . groupBy(_(0)).to(List).flatMap: (subject, defs) =>
            //               List
            //                (List(Tr(Td.extension(Code(Span(t"extension"))), Td.extension2(Code(subject.html)))),
            //                 node.terms.to(List).flatMap: (name, term) =>
            //                   val entry: Html["tr"] = Tr(Td.kind(keywords(term)), Th(Code(A(href = link(false))(name))))

            //                   List
            //                    (entry,
            //                     term.memo.let { memo => Tr(Td, Td.memo(memo.html)) }))
            //               . flatten

            //         val members: Seq[Html["tr"]] = node.terms.to(List).flatMap: (name, term) =>
            //           term.definition match
            //             case `extension`(_, _, _) => Nil
            //             case definition =>
            //               val entry: Html["tr"] = Tr(Td.kind(keywords(term)), Th(Code(A(href = link(false))(name))))

            //               List
            //                (entry,
            //                 term.memo.let { memo => Tr(Td, Td.memo(memo.html)) })

            //         members ++ grouped),

            //   detail.let(_.html).let(Div(H3(t"Documentation"), _)))

            "???")

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
           (Details.imports
             (Summary(B(t"import")),
              Div(Details(Summary(t"scala.*"))),
              Div(Details(Summary(t"scala.Predef.*"))),
              Div(Details(Summary(t"scala.collection.*")))),
            Details(Summary(H3(A(target = id"main", href = rootLocation)(pkg)))),
            Div:
              model(pkg).members.filter(!_(1).hidden).map: (member, node) =>
                node.tree(member.text, pkg, pkg+member.safe)),
          List(Iframe(id = id"api", name = t"main", src = rootLocation)))

    case _ =>
      Server.at(request.location).let(_.handle(using request)).or:
        Http.Response(NotFound(t"Not found"))
