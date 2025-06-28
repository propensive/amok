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

import executives.completions
import parameterInterpretation.posix
import unhandledErrors.stackTrace
import threadModels.platform
import supervisors.global
import asyncTermination.cancel
import httpServers.stdlibPublic
import logging.silent
import charEncoders.utf8
import charDecoders.utf8
import classloaders.threadContext
import strategies.throwUnsafely
import textSanitizers.skip
import systemProperties.jre
import workingDirectories.systemProperties

import html5.*

import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.disabled
import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled

given Tactic[CodlError] => Tactic[CodlReadError] => Translator =
  HtmlTranslator(AmokEmbedding(), ScalaEmbedding)

case class AmokError()(using Diagnostics) extends Error(m"There was a problem")

@main
def amok(): Unit = cli:
  var promises: Set[Promise[Unit]] = Set()
  execute:
    recover:
      case TerminalError() =>
        Out.println(m"Couldn't attach terminal")
        Exit.Fail(5)

      case WatchError() =>
        Out.println(m"Couldn't watch filesystem")
        Exit.Fail(5)

      case ServerError(port) =>
        Out.println(m"Couldn't listen on port $port")
        Exit.Fail(2)

      case NameError(a, b, c) =>
        Out.println(m"Invalid filename: ${a.toString} / ${b.toString} / ${c.toString}")
        Exit.Fail(4)
      case PathError(a, b) =>
        Out.println(m"Invalid filename ${a.toString} / ${b.toString}")
        Exit.Fail(4)

      case CodlError(_, _, _, _) =>
        Out.println(m"Bad content")
        Exit.Fail(3)

      case CodlReadError(_) =>
        Out.println(m"Could not read CoDL content")
        Exit.Fail(6)

      case AmokError() =>
        Out.println(m"There was an error")
        Exit.Fail(1)

    . within:
        val filename = arguments.prim.let(_()).lest(AmokError()).sub(t"./", t"")
        val file =
          if filename.starts(t"/") then filename.decode[Path on Linux]
          else workingDirectory[Path on Linux] + filename.decode[Relative on Linux]

        def load(): (doc: AmokDoc, content: List[Html["section"]]) = synchronized:
          Out.println(m"Loading $filename...")
          val fileText = file.open(_.read[Text])
          val stripped = fileText.cut(t"\n").dropWhile(_ != t"##").tail.join(t"\n")
          val doc = Codl.read[AmokDoc](fileText)
          val sections = Markdown.parse(stripped).broken

          (doc,
           sections.map(_.html).zipWithIndex.map: (content, index) =>
             Section(id = DomId(t"slide${index + 1}"))(content))

        var content = load()

        val server = tcp"8080".serve:
          request.target match
            case r"/$name([a-z]+).css" =>
              Http.Response(Classpath / "amok" / t"$name.css")

            case r"/favicon.ico" =>
              Http.Response(NotFound(t""))

            case r"/navigate.js" =>
              Http.Response(Classpath / "amok" / "navigate.js")

            case t"/update" =>
              val promise: Promise[Unit] = Promise()
              promises += promise

              promise.attend(30*Second)
              promises -= promise
              if !promise.ready then Http.Response(Http.Found)(t"") else
                Out.println(t"Updating presentation")
                 Http.Response(t"Updated")

            case _ =>
              Out.println(t"Serving HTTP request for $filename")
              Http.Response(cacheControl = t"no-store, no-cache, must-revalidate, max-age=0"):
                HtmlDoc:
                  Html
                   (Head
                     (html5.Link.Stylesheet(href = t"code.css"),
                      html5.Script(src = t"/navigate.js"),
                      Title(content(0).title)),
                    Body(Div.visible(id = id"overlay"), Main(content(1)*)))

        file.watch: watch =>
          interactive:
            Out.println(m"Presentation is being served at http://localhost:8080/")
            Out.println(m"Press [q] to quit")

            watch.stream.multiplex(terminal.events.stream)
            . takeWhile(_!= Keypress.CharKey('q')).each:
                case WatchEvent.Modify(file, _) =>
                  Out.println(m"Source file $file has changed")
                  val t0 = now()
                  content = load()
                  Out.println(m"New version prepared in ${now() - t0}")
                  val triggers = promises
                  promises = Set()
                  triggers.each(_.offer(()))

                case other =>
                  Out.println(m"Press [q] to quit")

        server.cancel()
        service.shutdown()
        Exit.Ok
