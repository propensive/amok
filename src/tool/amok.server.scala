package amok

import soundness.{is as _, Node as _, *}

def httpServer()(using Stdio): Unit raises ServerError raises ClasspathError = tcp"8080".serve:
  request.location match
    case _ /: t"api.css"  => Http.Response(Classpath/"amok"/"api.css")
    case _ /: t"utils.js" => Http.Response(Classpath/"amok"/"utils.js")
    case _ /: t"logo.svg" => Http.Response(Classpath/"amok"/"logo.svg")

    case _ /: t"entity" /: (name: Text) =>
      val (symbol, entity, node) = model.resolve(name)

      Http.Response:
        import html5.*
        try
          recover:
            case MarkdownError(_) | CodlError(_, _, _, _) | CodlReadError(_) =>
              Page.simple(H2(t"Error"), P(t"The page contained errors"))

          . within:
              import Markdown.renderable

              val detail: Optional[Markdown[Markdown.Ast.Block]] =
                node.detail.let(Markdown.parse(_))

              val index = Index.decode(name)

              Page.simple
               (index.only:
                  case Index.Entity(parent, isType, entity) =>
                    H1.pkg(Code(parent.html, symbol)),

                H1(Code(entity)),
                node.template.let: kind =>
                  val exts =
                    if kind.extensions.length == 0 then Unset
                    else Syntax.sequence(kind.extensions, Syntax.Comma).let(_.html)

                  Table.members
                   (Tr(Th(colspan = 2)(Code(kind.syntax.html, t" ", entity, t" extends ".unless(kind.extensions.isEmpty), exts))),
                    if node.types.isEmpty then Tr(Td(colspan = 2)((Em(t"This type has no members."))))
                    else node.types.to(List).flatMap: (name, template) =>
                      val link: Path on Rfc3986 = (% / "entity" / index.child(name, true).id).on[Rfc3986]
                      List
                        (Tr(Td.kind(Code(template.definition.let(_.syntax.html))),
                            Th(Code(A(href = link)(name)))),
                        template.memo.let { memo => Tr(Td, Td.memo(memo.html)) })),

                node.definition.let: kind =>
                  Table.members
                   (Tr(Th(colspan = 2)(Code.typed
                     (kind.syntax.html,
                      t" ",
                      Em(entity),
                      node.params.let(_.html),
                      t": ".unless(node.returnType.absent),
                      node.returnType.let(_.html)))),
                    if node.terms.isEmpty then List(Tr(Td(colspan = 2)(Em(t"This term has no members."))))
                    else
                      val extensions: List[(Syntax, Syntax, Text, Optional[InlineMd])] =
                        node.terms.to(List).flatMap: (name, node) =>
                          node.definition match
                            case `extension`(subject, definition, modifiers) =>
                              List((subject, definition.syntax, name, node.memo))
                            case _ =>
                              Nil

                      val grouped: Seq[Html["tr"]] =
                        extensions.groupBy(_(0)).to(List).flatMap: (subject, defs) =>
                          List
                           (List(Tr(Th(colspan = 2)(Code(t"extension ", subject.html)))),
                            defs.flatMap: (_, term, name, memo) =>
                              val link: Path on Rfc3986 = (% / "entity" / index.child(name, false).id).on[Rfc3986]

                              List
                               (Tr(Td.kind(Code(term.html)), Th(Code(A(href = link)(name)))),
                                memo.let { memo => Tr(Td, Td.memo(memo.html)) }))
                          . flatten

                      val members: Seq[Html["tr"]] = node.terms.to(List).flatMap: (name, term) =>
                        term.definition match
                          case `extension`(_, _, _) => Nil
                          case definition =>
                            val link: Path on Rfc3986 = (% / "entity" / index.child(name, false).id).on[Rfc3986]

                            List
                             (Tr(Td.kind(Code(definition.let(_.syntax).let(_.html))),
                              Th(Code(A(href = link)(name)))),
                              term.memo.let { memo => Tr(Td, Td.memo(memo.html)) })

                      members ++ grouped),

                detail.let(_.html).let(Div(_)))

        catch
          case exception: Throwable =>
            Page.simple(H1(t"Error"), Div(exception.stackTrace.html))

    case _ /: t"api" =>
      import html5.*
      val rootLocation: Path on Rfc3986 = % / "entity"

      Http.Response:
        Page
         (Nil,
          List
           (H2(t"All Packages"),
            Ul.all
             (model.root.members.filter(!_(1).hidden).map: (member, _) =>
                val link: Path on Rfc3986 = (% / "api" / member.text.skip(1)).on[Rfc3986]
                Li(Code(A(href = link)(member.text.skip(1)))))))

    case _ /: t"api" /: (pkg: Text) =>
      import html5.*

      val rootLocation: Path on Rfc3986 = % / "entity" / pkg

      Http.Response:
        Page
         (List
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
      Http.Response(t"Hello")
