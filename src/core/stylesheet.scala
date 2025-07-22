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
import cataclysm.*, pseudo.*
import gossamer.*
import honeycomb.*
import iridescence.*
import rudiments.*
import serpentine.*
import spectacular.*

object styles:

  val main: CssStylesheet = CssStylesheet()/*
    FontFace(fontFamily = t"\"Overpass\"", src = t"url(\"../fonts/overpass.ttf\")"),
    FontFace(fontFamily = t"\"Overpass Mono\"", src = t"url(\"../fonts/overpass-mono.ttf\")"),
    FontFace(fontFamily = t"\"Overpass Italic\"", src = t"url(\"../fonts/overpass-italic.ttf\")"),

    select(Body):
      Css(fontFamily = Font(t"Overpass"), margin = 0, padding = 0, overflowY = Overflow.Hidden,
          textAlign = TextAlign.Justify),

    select(Label):
      Css(fontWeight = 400, userSelect = UserSelect.None, fontFamily = Font(t"Overpass Mono"),
          fontVariantLigatures = t"none", fontSize = 0.9.em, width = 400.px,
          cursor = cataclysm.Cursor.Pointer, padding = (0.2.em, 4.5.em), marginLeft = -4.em),

    select(Header):
      Css(position = Position.Absolute, margin = 0, height = 6.em, width = 100.vw - 6.em,
          padding = (0, 0, 0, 6.em), backgroundColor = rgb"#111111",
          boxShadow = (0, 0, 1.em, rgb"#aaaaaa"), backgroundImage = % / p"images" / p"logo.svg",
          backgroundRepeat = BackgroundRepeat.NoRepeat, backgroundSize = 4.em,
          backgroundPosition = t"1em"),

    select(Header >> A || Header >> A&&hover || Header >> A&&visited || Header >> A&&active):
      Css(color = rgb"#dddddd", textDecoration = TextDecorationLine.None, fontWeight = 800,
          fontSize = 0.85.em),

    select(Header >> Ul):
      Css(margin = (1.4.em, 0)),

    select(Header >> Li):
      Css(display = Display.InlineBlock, padding = 1.em),

    select(Main):
      Css(position = Position.Absolute, height = 100.vh - 8.em, width = 58.vw - 4.em,
          padding = (0, 3.em, 0, 1.em), margin = (8.em, 0, 0, 42.vw), overflowY = Overflow.Scroll),

    select(Iframe):
      Css(width = 100.pc, height = 100.pc - 10.px, borderStyle = BorderStyle.None),

    select(Nav):
      Css(position = Position.Absolute, height = 100.vh - 8.em, margin = (8.em, 0, 0, 0),
          width = 38.vw, padding = (0, 1.em, 0, 3.em), overflowY = Overflow.Scroll,
          scrollbarWidth = t"none"),

    select(Nav&&webkitScrollbar):
      Css(width = 0),

    select(Ul):
      Css(listStyle = t"none", padding = 0),

    select(H1 >> Code):
      Css(fontFamily = Font(t"Overpass Mono"), fontWeight = 500, fontSize = 0.8.em),

    select(P):
      Css(fontWeight = 325, fontSize = 0.95.em, lineHeight = 1.5.em, color = rgb"#444444"),

    select(H2):
      Css(color = rgb"#555555", fontWeight = 400, fontSize = 1.2.em),

    select(Nav >> Li):
      Css(paddingLeft = 1.7.em, overflowX = Overflow.Hidden),

    select(Nav >> A):
      Css(color = rgb"#000000", textDecoration = TextDecorationLine.None),

    select(Nav >> A&&hover):
      Css(textDecoration = TextDecorationLine.Underline),

    select(Li >> Label):
      Css(lineHeight = 1.5.em, backgroundImage = % / p"images" / p"type.svg",
          backgroundRepeat = BackgroundRepeat.NoRepeat, backgroundPosition = (3.5.em, -0.1.em),
          backgroundSize = 1.3.em, overflowX = Overflow.Hidden,
          padding = (0.em, 0.em, 0.em, 5.5.em)),

    select(Nav >> Li && pseudo.has(Div)):
      Css(backgroundImage = % / p"images" / p"more.svg", backgroundSize = 1.3.em,
          backgroundRepeat = BackgroundRepeat.NoRepeat, backgroundPosition = (-0.35.em, -0.1.em)),

    select(Nav >> Li && pseudo.has(Div) && pseudo.has(Input&&checked)):
      Css(backgroundImage = % / p"images" / p"less.svg", backgroundSize = 1.3.em,
          backgroundRepeat = BackgroundRepeat.NoRepeat, backgroundPosition = (-0.35.em, -0.1.em)),

    select(Nav >> Li >> Ul):
      Css(marginLeft = 0.em),

    select(Nav >> Input):
      Css(borderStyle = BorderStyle.None, borderBottom = (BorderStyle.Solid, 1.px, rgb"#777777"),
          fontFamily = Font(t"Overpass Mono"), fontSize = 0.9.em, width = 20.em,
          marginLeft = -0.3.em, padding = (0, 0, 0.1.em, 1.9.em),
          backgroundImage = % / p"images" / p"filter.svg", backgroundSize = 1.3.em,
          backgroundRepeat = BackgroundRepeat.NoRepeat, backgroundPosition = (0.1.em, -0.2.em)),

    select(Nav >> Input&&focus):
      Css(borderColor = rgb"#000000", outline = t"none"),

    select(Ul >> Input):
      Css(display = Display.None),

    select(Li && hover):
      Css(fontWeight = 700),

    select(Ul >> Input ~ Div):
      Css(minHeight = Inherit, height = Inherit, maxHeight = 0.px, overflowY = Overflow.Hidden),

    select(Ul >> Input&&checked ~ Div):
      Css(minHeight = 20.px, height = Inherit, maxHeight = Inherit),

    select(Nav >> H3):
      Css(fontSize = 0.75.em, color = rgb"#666666", fontWeight = 600, margin = (0.6.em, 0, 0.2.em, 1.9.em)),

    MediaRule(t"only screen and (max-width: 1000px)")(
      select(Main):
        Css(height = 50.vh, margin = (50.vh, 0, 0, 0), width = 100.vw - 4.em,
            borderTop = (BorderStyle.Solid, 1.px, rgb"#dddddd")),

      select(Nav):
        Css(height = 50.vh - 8.em, margin = (8.em, 0, 0, 0), width = 100.vw - 4.em)
    ),

    MediaRule(t"only screen and (max-device-width: 768px)")(
      select(Main):
        Css(height = 50.vh, margin = (50.vh, 0, 0, 0), width = 100.vw - 4.em,
            borderTop = (BorderStyle.Solid, 1.px, rgb"#dddddd")),

      select(Nav):
        Css(height = 50.vh - 8.em, margin = (8.em, 0, 0, 0), width = 100.vw - 4.em)
    )
  )
*/

object Icons:

  /*
    c: case
    e: extension
    p: param
    t: transparent
    a: abstract
    i: inline
    o: opaque/open
    s: sealed
    f: final
    l: lazy
  */

  enum Qualifier:
    case Extension, Param, Transparent, Sealed, Abstract, Inline, OpaqueOrOpen, Final, Lazy, Case

  enum Entity:
    case Package, Def, Trait, Given, Enum, Case, Class, Val, Var, Type, Object, Cclass

  case class Icon(entity: Entity, qualifiers: List[Qualifier]):
    def filename: Text =
      val entityName = t"${entity.toString.show.lower}.svg"

      if qualifiers.isEmpty then entityName
      else t"${qualifiers.sortBy(_.ordinal).map(_.toString.show.keep(1)).join.lower}-$entityName"
