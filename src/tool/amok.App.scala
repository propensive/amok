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

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

given Tactic[CodlError] => Tactic[CodlReadError] => Translator =
  HtmlTranslator(AmokEmbedding(false), ScalaEmbedding)

val About  = Subcommand(t"about",  e"find out about Amok")
val Load   = Subcommand(t"load",   e"load definitions from a .jar or .amok file")
val Boot   = Subcommand(t"boot",   e"start Amok using the .amok file in the current directory")
val Clear  = Subcommand(t"clear",  e"clear definitions from a JAR file")
val Quit   = Subcommand(t"quit",   e"shutdown Amok")
val Serve  = Subcommand(t"serve",  e"serve the documentation on a local HTTP server")

var model = Model()

@main
def application(): Unit = cli:
  idempotent(effectful(safely(TabCompletions.install())))

  arguments match
    case About() :: _ => execute:
      recover:
        case SerializationError(_, _) => panic(m"Failed to deserialize")

      . within:
          Out.println()
          t"""H4sIADMTXWgAA51RwQ3AIAj8OwWjNr77lDFqnYlJGoO2ojaChhjEu+NQAO0ivCgcJTA6NREoeN63OKLvGBh7Z4
              RnhxgroFCtDobJCdMEX7DMYgZX23yO+KrBZ694/89e71p/pqx8Zu7iFl6smdR51ZPd95oz+4XunQTCet5RdA9q
              ts7STgMAAA=="""
          . erase(' ', '\n')
          . deserialize[Base64]
          . gunzip
          . utf8
          . cut(t"\n")
          . each: line =>
              Out.println(line)

          given Decimalizer(significantFigures = 4, exponentThreshold = Unset)
          val memory = Heap.used/1.mib
          val build = unsafely((Classpath / "build.id").read[Text].trim)

          Out.println(e"$Bold(Amok) prerelease version, build $build: $Italic(a documentation compiler for Scala)")
          Out.println(e"© Copyright 2025, Propensive OÜ")
          Out.println()
          Out.println(e"Memory usage: $memory MiB")
          Out.println()

      Exit.Ok

    case Load() :: Pathname(file) :: _ =>
      execute(load(file))

    case Clear() :: Nil =>
      execute:
        model = Model()
        Out.println(m"Documentation database has been cleared")
        Exit.Ok

    case Quit() :: _ => execute(service.shutdown() yet Exit.Ok)

    case Serve() :: _ => execute:
      recover:
        case ServerError(port) =>
          Out.println(m"Can't start a server on port $port") yet Exit.Fail(1)
        case ClasspathError(path) =>
          panic(m"Expected to find $path on the classpath")

      . within:
          httpServer()
          Out.println(e"Listening on $Bold(http://localhost:8080)")
          Exit.Ok

    case Boot() :: _ => execute:
      load(unsafely(workingDirectory[Path on Linux]) / ".amok")

    case command :: _ =>
      execute(Out.println(m"Unknown command: ${command()}") yet Exit.Fail(1))

    case Nil =>
      execute(Out.println(m"Please specify a command") yet Exit.Fail(1))


def load(file: Path on Linux)(using Stdio): Exit =
  if file.name.ends(t".jar") then
    recover:
      case IoError(_, _, _) => Exit.Fail(1)
      case StreamError(_)   => Exit.Fail(1)

    . within:
        model.load(file)
        Exit.Ok
  else if file.name.ends(t".amok") then
    recover:
      case exception: Error =>
        Out.println(exception.stackTrace.teletype)
        Exit.Fail(1)

    . within:
        model.overlay(Amox.read(file))
        Exit.Ok

  else
    Out.println(m"Path $file was not of the right type")
    Exit.Fail(1)
