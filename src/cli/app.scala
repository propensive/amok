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
import turbulence.*
import spectacular.*
import gossamer.*
import vacuous.*
import anticipation.*, fileApi.galileiApi
import fulminate.*
import perforate.*
import galilei.*, filesystemOptions.{doNotCreateNonexistent, dereferenceSymlinks}
import serpentine.*, hierarchies.unix
import ambience.*, environments.jvm, homeDirectories.default

given (using Cli): WorkingDirectory = workingDirectories.daemonClient 

case class AmokError(details: Message) extends Error(details)

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
              
              val absolute = safely(file2.decodeAs[Path]).or:
                workingDirectory.or(abort(AmokError(msg"There is no working directory"))) + file2.decodeAs[Unix.Link]
              
              Out.println(t"Hello world!")
              Out.println(t"The classpath is ${classpath.or(t"unknown")}")
              Out.println(t"The file is ${absolute}")
              ExitStatus.Ok

          case _ =>
            execute:
              Out.println(t"Unknown command")
              ExitStatus.Fail(1)
