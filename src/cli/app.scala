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

import escapade.*
import parasite.*
import spectral.*
import rudiments.*
import exoskeleton.*, executives.completions, unhandledErrors.stackTrace, parameterInterpretation.posix
import eucalyptus.*
import anthology.*
import turbulence.*
import spectacular.*
import gossamer.*
import cellulose.*, codlPrinters.standard
import vacuous.*
import anticipation.*, fileApi.galileiApi
import fulminate.*
import perforate.*
import punctuation.*
import hellenism.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import serpentine.*, hierarchies.unix
import hieroglyph.*, charDecoders.utf8, badEncodingHandlers.strict
import ambience.*, environments.jvm, homeDirectories.default, systemProperties.jvm

given (using Cli): WorkingDirectory = workingDirectories.daemonClient 

case class AmokError(details: Message) extends Error(details)
case class Fragment(language: Language)
case class Language(name: Text, version: Text)

@main
def main(): Unit =
  unsafely:
    supervise:
      given Log[Output] = logging.silent[Output]
      
      val Classpath = Flag[Text](t"classpath", false, List('c'), t"specify the classpath")
      val File = Flag[Text](t"file", false, List('f'), t"specify a file to check")
      val Install = Subcommand(t"install", t"install the application")
      val Check = Subcommand(t"check", t"check a markdown file")

      daemon:
        safely(arguments.head) match
          case Install() =>
            execute:
              Out.println(Installer.install().communicate)
              Out.println(TabCompletions.install(force = true).communicate)
              ExitStatus.Ok
          
          case Check() =>
            val classpath = Classpath()
            val file = File()
            execute:
              val file2 = file.or(abort(AmokError(msg"The file has not been specified")))
              val classpath2 = classpath.or(abort(AmokError(msg"The classpath has not been specified")))
              val classpath3 = classpath2.cut(t":").map: path =>
                safely(path.decodeAs[Path]).or(path.decodeAs[Unix.Link].inWorkingDirectory)

              val markdown = safely(file2.decodeAs[Path]).or(file2.decodeAs[Unix.Link].inWorkingDirectory).as[File]
              val content = markdown.readAs[Text]
              
              Out.println(t"The classpath is ${classpath3.debug}")
              Out.println(t"The file is ${markdown.path}")
              
              Out.println(Fragment(Language(t"test", t"1.0")).codl.show)
              Out.println(Fragment(Language(t"test", t"1.0")).codl.schema.debug)
              Out.println(Fragment(Language(t"test", t"1.0")).codl.children.debug)
              
              val fragments: Seq[Text] = Markdown.parse(content).nodes.collect:
                case Markdown.Ast.Block.FencedCode(Some(t"amok"), meta, code) =>
                  val codl: CodlDoc = Codl.parse(code)
                  Out.println(codl.children.debug)
                  Out.println(codl.show)
                  safely(codl.as[Fragment]).or(Fragment(Language(t"unknown", t"0.0")))
                  codl.body.foldLeft(t"")(_ + _.show)

              for fragment <- fragments do
                Out.println(t"Compiling...")
                Compilation(Map(t"fragment" -> fragment), classpath3, workingDirectory)().foreach: diagnostic =>
                  Out.println(diagnostic.toString.tt)

              ExitStatus.Ok

          case _ =>
            execute:
              Out.println(t"Unknown command")
              ExitStatus.Fail(1)
