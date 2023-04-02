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

import deviation.*
import gesticulate.*
import gossamer.*
import honeycomb.*
import rudiments.*
import serpentine.*
import cataclysm.*

object pages:
  val count: Counter = Counter(0)

  def render(docs: Docs, prefix: Text = t"i", path: List[Text] = Nil): List[Element["li"]] =
    if docs.term.values.size + docs.`type`.values.size > 0
    then (docs.term.values ++ docs.`type`.values).to(List).sortBy(_.name).zipWithIndex.map:
      case (item, idx) =>
        val empty = item.empty
        val location = unsafely(^ / p"info" / path.reverse.join(t"", t".", t".${item.name}"))
        
        Li(tabindex = count(), hclass = if empty then cls"" else styles.more)(
          Label(`for` = t"$prefix-$idx", style = Css(backgroundImage = unsafely(^ / p"images" / item.icon)))(A(href = location, target = t"main")(item.name)),
          Input(id = t"$prefix-$idx", htype = HType.Checkbox),
          if empty then Nil else List(Ul(render(item, t"$prefix-$idx", item.name :: path)))
        )
    else Nil
  
  def main(docs: Docs): HtmlDoc = HtmlDoc(
    Html(
      Head(
        Title(t"Amok Documentation"),
        Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css"),
        Link(rel = Rel.Icon, htype = media"image/svg+xml", href = ^ / p"images" / p"logo.svg")
      ),
      Body(
        Header(Ul(
          Li(A(href = ^)(t"HOME")),
          Li(A(href = ^ / p"about")(t"ABOUT AMOK")),
          Li(A(href = ^ / p"ref")(t"REFERENCE")),
          Li(A(href = ^ / p"kill")(t"CONTRIBUTE"))
        )),
        Main(Iframe(name = t"main", src = ^ / p"info" / p"welcome")),
        Nav(
          H2(t"API Documentation"),
          Input(name = t"filter"),
          Ul(render(docs))
        ),
        Footer()
      )
    )
  )
  
  def info(name: Text) = HtmlDoc(
    Html(
      Head(
        Title(t"$name"),
        Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css")
      ),
      Body(
        H1(Code(t"$name")),
        H2(t"About Amok"),
        P(t"Welcome to Amok, an API tool for Scala and other languages. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
        Pre(t"This is some code.")
      )
    )
  )

