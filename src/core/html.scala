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

  def render(node: DocNode, prefix: Text, path: DocPath, isType: Boolean): List[Element["li"]] =
    (node.terms.keys ++ node.types.keys).to(List).sorted.zipWithIndex.map: (name, idx) =>
      val list: List[Element[Flow]] = if node.terms.contains(name) && node.types.contains(name) then
        println("Mixed: "+name+" "+node.terms(name).info+" "+node.types(name).info)
        val terms = render(node.terms(name), t"$prefix-$idx", path / name, false)
        val types = render(node.types(name), t"$prefix-$idx", path /# name, true)
        
        List(
          if types.isEmpty then Nil else List(Ul(types)),
          if terms.isEmpty then Nil else List(H3(t"Companion")),
          if terms.isEmpty then Nil else List(Ul(terms))
        ).flatten

      else if node.terms.contains(name) then
        val terms = render(node.terms(name), t"$prefix-$idx", path / name, false)
        if terms.isEmpty then Nil else List(Ul(terms))
      else
        val types = render(node.types(name), t"$prefix-$idx", path /# name, true)
        if types.isEmpty then Nil else List(Ul(types))
      
      val newPath = if node.terms.isEmpty then path /# name else path / name
      
      val info = node.types.get(name).getOrElse(node.terms(name)).info
      
      Li(tabindex = count())(
        Label(`for` = t"$prefix-$idx", style = Css(backgroundImage = unsafely(^ / p"images" / info.option.map(_.icon.filename).getOrElse(t"object.svg"))))(
          A(href = unsafely(^ / p"info" / newPath.url), target = t"main")(name)
        ),
        Input(id = t"$prefix-$idx", htype = HType.Checkbox),
        if list.isEmpty then Nil else List(Div(list))
      )

  def template(title: Text, body: Element["header" | "main" | "nav"]*): HtmlDoc = HtmlDoc(
    Html(
      Head(
        Title(t"Amok: $title"),
        Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css"),
        Link(rel = Rel.Icon, htype = media"image/svg+xml", href = ^ / p"images" / p"logo.svg")
      ),
      Body(List(body*))
    )
  )

  val main: HtmlDoc = HtmlDoc(
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
          Ul(render(DocNode.root, t"i", DocPath(Nil), false))
        ),
        Footer()
      )
    )
  )
  
  def info(path: DocPath): HtmlDoc =
    DocNode.unapply(path) match
      case None =>
        HtmlDoc(
          Html(
            Head(
              Title(t"Amok: not found ${path.text}"),
              Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css")
            ),
            Body(H1(t"Not Found: ${path.text}"))
          )
        )
      case Some(node) =>
        HtmlDoc(
          Html(
            Head(
              Title(t"Amok: ${path.text}"),
              Link(rel = Rel.Stylesheet, href = ^ / p"styles" / p"amok.css")
            ),
            Body(
              H2(Code(path.init.text)),
              H1(Code(t"${path.last}")),
              H2(t"About Amok"),
              P(t"Welcome to Amok, an API tool for Scala and other languages. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata sanctus est Lorem ipsum dolor sit amet."),
              Pre(t"This is some code.")
            )
          )
        )

