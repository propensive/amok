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
import errorDiagnostics.stackTraces
import textMetrics.uniform
import tableStyles.horizontal
import columnAttenuation.ignore

val About   = Subcommand("about",   e"find out about Amok")
val Load    = Subcommand("load",    e"load definitions from a .jar or .amok file")
val Boot    = Subcommand("boot",    e"start Amok using the .amok file in the current directory")
val Clear   = Subcommand("clear",   e"clear definitions from a JAR file")
val Quit    = Subcommand("quit",    e"shutdown Amok")
val Serve   = Subcommand("serve",   e"serve the documentation on a local HTTP server")
val Folios  = Subcommand("list",    e"list all deployed mountpoints")

val MountpointArg = Flag[Mountpoint]("mountpoint", false, List('m'), "the URL at which to serve the folio, e.g. /tutorials")

@main
def application(): Unit = cli:
  // Try to install tab-completions only once
  Completions.ensure()

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
          . each(Out.println(_))

          given Decimalizer(significantFigures = 4, exponentThreshold = Unset)
          val memory = Heap.used/1.mib
          val build = unsafely((Classpath / "build.id").read[Text].trim)

          Out.println(e"$Bold(Amok) prerelease version, build $build: $Italic(a documentation engine for Scala)")
          Out.println(e"© Copyright 2025 Propensive OÜ")
          Out.println()
          Out.println(e"Memory usage: $memory MiB")
          Out.println()

      Exit.Ok

    case Folios() :: _ => execute:
      val table =
        Table[Folio]
         (Column(e"$Bold(Mountpoint)")(_.base.show),
          Column(e"$Bold(Source)")(_.source.relativeTo(unsafely(workingDirectory[Path on Linux])).encode),
          Column(e"$Bold(Type)")(_.kind))

      recover:
        case TerminalError() =>
          Out.println(table.tabulate(Server.folios).grid(100))
      . within:
          interactive:
            Out.println(table.tabulate(Server.folios).grid(terminal.columns.or(100)))

      Exit.Ok

    case Load() :: Pathname(file) :: _ =>
      safely(MountpointArg())
      execute:
        try
          recover:
            case error@LoadError(_, _) =>
              Out.println(error.message)
              Exit.Fail(1)
          . within:
              val mountpoint = MountpointArg().or(Mountpoint())
              Out.println(m"Deploying $file to $mountpoint".teletype)
              Server.register(Folio.load(mountpoint, file))
              Exit.Ok

        catch case error: Throwable =>
          Out.println(error.stackTrace.teletype)
          Out.println(m"Error: ${error.toString}") yet Exit.Ok

    case Quit() :: _ => execute(service.shutdown() yet Exit.Ok)

    case Serve() :: _ => execute:
      recover:
        case ServerError(port) =>
          Out.println(m"Can't start a server on port $port") yet Exit.Fail(1)

        case ClasspathError(path) =>
          panic(m"Expected to find $path on the classpath")

      . within:
          tcp"8080".serve:
            Server.at(request.location).lay(Http.Response(NotFound(Page.simple(Mountpoint(), t"There is no page at ${request.location}")))): folio =>
              folio.handle(using request)

          Out.println(e"Listening on $Bold(http://localhost:8080)")
          Exit.Ok

    case command :: _ =>
      execute(Out.println(m"Unknown command: ${command()}") yet Exit.Fail(1))

    case Nil =>
      execute(Out.println(m"Please specify a command") yet Exit.Fail(1))
