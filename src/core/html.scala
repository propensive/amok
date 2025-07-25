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

import anticipation.*
import cataclysm.*
import contingency.*
import digression.*
import gesticulate.*
import gossamer.*
import honeycomb.*
import rudiments.*
import serpentine.*, pathHierarchies.simple
import vacuous.*

object pages:
  val count: Counter = Counter(0)

  def render(db: Db, prefix: Text, path: Path, italic: Boolean = false): List[Element["li"]] =
    db.children(path).zipWithIndex.map: (item, idx) =>
      val info = db.info(item).option.get
      val typeChildren = render(db, t"$prefix-$idx", item.asType, true)
      val termChildren = render(db, t"$prefix-$idx", item.asTerm)
      Li(
        Label(`for` = t"$prefix-$idx", style = Css(backgroundImage = unsafely(% / n"images" / info.icon.filename)))(
          A(href = unsafely(% / n"info" / item.url), target = t"main")(if italic then Em(item.id) else B(item.id))
        ),
        Input(id = t"$prefix-$idx", `type` = Type.Checkbox),
        if typeChildren.isEmpty && termChildren.isEmpty then Nil else List(Div(
          List(
            if typeChildren.isEmpty then Nil else List(Ul(typeChildren)),
            if termChildren.isEmpty then Nil else List(Ul(termChildren))
          ).flatten
        ))
      )

  def template(title: Text, body: Element["header" | "main" | "nav"]*): HtmlDoc = HtmlDoc(
    Html(
      Head(
        Title(t"Amok: $title"),
        Link(rel = Rel.Stylesheet, href = % / n"styles" / n"amok.css"),
        Link(rel = Rel.Icon, htype = media"image/svg+xml", href = % / n"images" / n"logo.svg")
      ),
      Body(List(body*))
    )
  )

  def main(db: Db): HtmlDoc = HtmlDoc(
    Html(
      Head(
        Title(t"Amok Documentation"),
        Link(rel = Rel.Stylesheet, href = % / n"styles" / n"amok.css"),
        Link(rel = Rel.Icon, htype = media"image/svg+xml", href = % / n"images" / n"logo.svg")
      ),
      Body(
        Header(Ul(
          Li(A(href = %)(t"HOME")),
          Li(A(href = % / n"about")(t"ABOUT AMOK")),
          Li(A(href = % / n"ref")(t"REFERENCE")),
          Li(A(href = % / n"kill")(t"CONTRIBUTE"))
        )),
        Main(Iframe(name = t"main", src = % / n"info" / n"welcome")),
        Nav(
          H2(t"API Documentation"),
          Input(name = t"filter"),
          Ul(render(db, t"i", Path.Root))
        ),
        Footer()
      )
    )
  )

  def info(db: Db, path: Identifier): HtmlDoc =
    db.info(path).option match
      case None =>
        HtmlDoc(
          Html(
            Head(
              Title(t"Amok: not found ${path.text}"),
              Link(rel = Rel.Stylesheet, href = % / n"styles" / n"amok.css")
            ),
            Body(H1(t"Not Found: ${path.text}"))
          )
        )
      case Some(node) =>
        HtmlDoc(
          Html(
            Head(
              Title(t"Amok: ${path.text}"),
              Link(rel = Rel.Stylesheet, href = % / n"styles" / n"amok.css")
            ),
            Body(
              H2(Code(path.path.text)),
              H1(Code(t"${path.id}")),
              H2(t"About Amok"),
              P(t"Welcome to Amok, an API tool for Scala and other languages. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
              Pre(t"This is some code.")
            )
          )
        )
