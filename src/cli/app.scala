/*
    Amok, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import escapade.*
import parasite.*, threadModels.virtual, asyncOptions.waitForOrphans
import ethereal.*
import rudiments.*
import surveillance.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import eucalyptus.*
import anthology.*
import profanity.*
import quantitative.*
import turbulence.*
import monotonous.*, alphabets.base64.unpadded
import spectacular.*
import gossamer.*
import symbolism.*
import cellulose.*
import vacuous.*
import anticipation.*, filesystemApi.galileiPath, durationApi.aviationDuration, instantApi.aviationInstant
import fulminate.*
import contingency.*
import harlequin.*, syntaxHighlighting.numbered
import punctuation.*
import hallucination.*
import hellenism.*, classloaders.threadContext
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import serpentine.*, hierarchies.unix
import hieroglyph.*//, charDecoders.utf8, encodingMitigation.strict
import ambience.*, environments.virtualMachine, homeDirectories.default, systemProperties.virtualMachine

given (using Cli): WorkingDirectory = workingDirectories.daemonClient
given [EventType: Communicable] => EventType is Transcribable into Message = _.communicate
given Message is Loggable = Log.silent[Message]

object Errors:
  given decoder(using Errant[EnumCaseError]): Decoder[Errors] =
    case t"fail"      => Errors.Fail
    case t"ignore"    => Errors.Ignore
    case t"highlight" => Errors.Highlight
    case t"show"      => Errors.Show
    case other        => raise(EnumCaseError(other))(Errors.Ignore)

  given encoder: Encoder[Errors] = _.toString.tt.lower

enum Errors:
  case Fail, Ignore, Highlight, Show

case class AmokError(details: Message) extends Error(details)
case class Fragment
    (id:       Optional[Text],
     language: Optional[Language],
     errors:   Optional[Errors]   = Unset,
     follows:  Optional[Text]     = Unset)
case class Language(compiler: Text, version: Text)


@main
def main(): Unit =
  unsafely:
    supervise:
      object params:
        val Classpath = Flag[Text](t"classpath", false, List('c'), t"specify the classpath")
        val File = Flag[Text](t"file", false, List('f'), t"specify a file to check")
        val Watch = Switch(t"watch", false, List('w'), t"watch for changes")
        val Install = Subcommand(t"install", t"install the application")
        val Check = Subcommand(t"check", t"check a markdown file")
        val About = Subcommand(t"about", t"information about this release of Amok")
        val Shutdown = Subcommand(t"shutdown", t"stop Amok running as a background process")

      cliService:
        safely(arguments.head) match
          case params.Install() =>
            execute:
              Out.println(Installer.install().communicate)
              Out.println(TabCompletions.install(force = true).communicate)
              ExitStatus.Ok

          case params.About() =>
            execute:
              Out.println(Image((Classpath / p"logo.png")()).render)

              t"ICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICDila3ilIDilIDila4KICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICDilIIgIOKUggogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIOKUgiAg4pSCCuKVreKUgOKUgOKUgOKUgOKUgOKUgOKUgOKVruKVreKUgOKUgOKVruKUgOKUgOKUgOKUgOKVruKUgOKUgOKUgOKUgOKVruKVreKUgOKUgOKUgOKUgOKUgOKUgOKUgOKVruKUgiAg4pSC4pWt4pSA4pSA4pWuCuKUgiAg4pWt4pSA4pWuICDilILilIIgIOKVreKUgOKVriAg4pWt4pSA4pWuICDilILilIIgIOKVreKUgOKVriAg4pSC4pSCICDilbDila8gLuKVrwrilIIgIOKUgiDilIIgIOKUguKUgiAg4pSCIOKUgiAg4pSCIOKUgiAg4pSC4pSCICDilIIg4pSCICDilILilIIgIOKVreKVriDilbDila4K4pSCICDilbDilIDila8gIOKUguKUgiAg4pSCIOKUgiAg4pSCIOKUgiAg4pSC4pSCICDilbDilIDila8gIOKUguKUgiAg4pSC4pSCICDilIIK4pWw4pSA4pSA4pSA4pSA4pWv4pSA4pSA4pWv4pWw4pSA4pSA4pWvIOKVsOKUgOKUgOKVryDilbDilIDilIDila/ilbDilIDilIDilIDilIDilIDilIDilIDila/ilbDilIDilIDila/ilbDilIDilIDila8K".deserialize[Base64].utf8.cut(t"\n").each: line =>
                Out.print(t" "*18)
                Out.println(line)

              ExitStatus.Ok

          case params.Check() =>
            params.Classpath()
            params.File()
            params.Watch()

            execute:
              val file = params.File().or(abort(AmokError(msg"The file has not been specified")))

              val classpath: LocalClasspath = LocalClasspath:
                params.Classpath().or(abort(AmokError(msg"The classpath has not been specified"))).cut(t":").to(List).map: path =>
                  val path2 = safely(path.decodeAs[Unix.Path]).or(path.decodeAs[Unix.Link].inWorkingDirectory)
                  if path2.is[Directory] then ClasspathEntry.Directory(path2.show)
                  else ClasspathEntry.Jarfile(path2.show)

              val markdownFile = safely(file.decodeAs[Path]).or(file.decodeAs[Unix.Link].inWorkingDirectory).as[File]

              def recompile(): Unit =
                import charDecoders.utf8, encodingMitigation.strict
                val markdown = markdownFile.readAs[Text]

                val fragments: Seq[(Fragment, Text)] =
                  Markdown.parse(markdown).nodes.collect:
                    case Markdown.Ast.Block.FencedCode(t"scala", meta, code) =>
                      val fragment = Fragment(t"id", Language(t"scala", t"3.3"))
                      fragment -> code

                    case Markdown.Ast.Block.FencedCode(t"amok", meta, code) =>
                      val codl: CodlDoc = Codl.parse(code)

                      given AmokError mitigates EnumCaseError =
                        case EnumCaseError(enumCase) => AmokError(msg"Bad enum case: $enumCase")

                      given AmokError mitigates AggregateError[?] = error =>
                        AmokError(msg"Could not read fragment")

                      val fragment: Fragment = Codl.read[Fragment](code)

                      Out.println(fragment.debug)
                      fragment -> codl.body.foldLeft(t"")(_ + _.show)

                val allCode: Text = fragments.map(_(1)).join
                val highlighted: ScalaSource = ScalaSource.highlight(allCode)
                val notices = Scalac[3.4](List())(classpath)(Map(t"fragments" -> allCode), workingDirectory).notices

                def assign(codeSize: Int, todo: List[(Fragment, Text)]): Unit = todo match
                  case Nil =>
                    ()

                  case (fragment, code) :: more =>

                    notices.each: notice =>
                      notice.codeRange.let: codeRange =>
                        Out.println(codeRange.of(highlighted).teletype)
                        Out.println(notice.message)

                    assign(codeSize+code.length, more)

                assign(0, fragments.to(List))

                val errorCount: Teletype = notices.length match
                  case 0 => e"no errors"
                  case 1 => e"$Bold(one) error"
                  case 2 => e"$Bold(two) errors"
                  case 3 => e"$Bold(three) errors"
                  case 4 => e"$Bold(four) errors"
                  case n => e"$Bold($n) errors"

                Out.println(e"$Italic(Checked ${fragments.length} fragments, $errorCount)")

              terminal:
                def loop(stream: LazyList[TerminalEvent | Update.type], noChange: Boolean): Unit =
                  if !noChange then
                    recompile()
                    Out.println(e"Waiting for changes...")

                    if params.Watch().present
                    then Out.println(e"$Italic[(Press $Bold[Ctrl+C] or $Bold[Esc] to exit)]")

                  stream match
                    case (Keypress.Ctrl('C') | Keypress.Escape) #:: _ => ()
                    case Update #:: more                                 => loop(more, false)
                    case _ #:: more                                      => loop(more, true)
                    case _                                               => ()

                if params.Watch().absent then loop(LazyList(), false) else
                  val path = markdownFile.path

                  path.parent.vouch(using Unsafe).watch: changes =>
                    val fileChanges = changes.stream.cluster(0.1*Second).map(_.map(_.path.fullname).to(Set))
                    val relevantChanges = fileChanges.filter(_.contains(path.fullname)).map(Update.waive)
                    loop(relevantChanges.multiplexWith(terminal.eventStream()), false)

              ExitStatus.Ok

          case params.Shutdown() =>
            execute:
              service.shutdown()
              ExitStatus.Ok

          case _ =>
            execute:
              Out.println(t"Unknown command")
              ExitStatus.Fail(1)


object Update
