                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                               ╭───╮                              ┃
┃                                                               │   │                              ┃
┃                                                               │   │                              ┃
┃                        ╭─────────╮╭───╮╌────╮╌────╮╭─────────╮│   │ ╭───╮                        ┃
┃                        ╰─────╮   ││   ╭─╮   ╭─╮   ││   ╭─╮   ││   │╌╯   │                        ┃
┃                        ╭─────╯   ││   │ │   │ │   ││   │ │   ││        ╌╯                        ┃
┃                        │   ╭─╮   ││   │ │   │ │   ││   │ │   ││   ╭─╮   │                        ┃
┃                        │   ╰─╯   ││   │ │   │ │   ││   ╰─╯   ││   │ │   │                        ┃
┃                        ╰─────────╯╰───╯ ╰───╯ ╰───╯╰─────────╯╰───╯ ╰───╯                        ┃
┃                                                                                                  ┃
┃    Amok, version 0.1.0.                                                                          ┃
┃    © Copyright 2022-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://amok.propensive.com/                                                              ┃
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

import soundness.*
import telekinesis.*

import strategies.throwUnsafely
import httpServers.stdlib
import asyncTermination.cancel
import logging.silent
import charEncoders.utf8
import supervisors.global

import html5.*

object Tests extends Suite(m"Amok Tests"):
  def run(): Unit =
    given HtmlConverter = HtmlConverter(AmokRenderer())

    val markdown = md"Hello _world_"
    println(markdown)
    tcp"8080".serve[Http]:
      Http.Response:
        HtmlDoc(Html(Head(Title(t"Example")), Body(markdown.html)))


    snooze(10.0*Second)

    // test(t"root package"):
    //   Path.Root.text
    // . assert(_ == t"_root_")

    // test(t"package name"):
    //   Path.Term(Path.Root, t"escritoire").text
    // . assert(_ == t"escritoire")

    // test(t"class name"):
    //   Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column").text
    // . assert(_ == t"escritoire.Column")

    // test(t"class method name"):
    //   Path.Term(Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column"), t"apply").text
    // . assert(_ == t"escritoire.Column#apply")

    // test(t"Read simple name"):
    //   Identifier(t"escritoire.Column")
    // . assert(_ == Identifier(Path.Term(Path.Root, t"escritoire"), t"Column"))

    // test(t"Read class method"):
    //   Identifier(t"escritoire.Column#width")
    // . assert(_ == Identifier(Path.Type(Path.Term(Path.Root, t"escritoire"), t"Column"), t"width"))

    // val examples =
    //   List
    //    (t"escritoire.Column#apply",
    //     t"escritoire.Column.width",
    //     t"Column#Cell#width",
    //     t"Column#Cell.apply")

    // for eg <- examples do test(t"Roundtrip test: $eg"):
    //   Identifier(eg).text
    // . assert(_ == eg)
