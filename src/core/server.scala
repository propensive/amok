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

import cellulose.*
import digression.*
import galilei.*
import gossamer.*
import anticipation.*, fileApi.galileiApi
import scintillate.*
import spectacular.*
import serpentine.*, hierarchies.simple
import parasite.*, monitors.global
import rudiments.*
import eucalyptus.*, logging.stdout
import turbulence.*, basicIo.jvm
import hieroglyph.*, charEncoders.utf8

import unsafeExceptions.canThrowAny

@main
def run(): Unit =
  try
    val db = unsafely:
      val dirs = List(t"/home/propensive/work/amok/out".decodeAs[Path].as[Directory])
      val tastyFiles = dirs.flatMap(_.descendants.filter(_.name.ends(t".tasty")).files)
      Amok.inspect(tastyFiles)
    
    lazy val server: ActiveServer = HttpServer(8080).listen:
      request.path match
        case % / p"styles" / p"amok.css" => Response(styles.main)
        case % / p"fonts" / name         => Response(Ttf(data.font(name)))
        case % / p"images" / name        => Response(Svg(data.image(name)))
        case % / p"info" / path          => Response(pages.info(db, Name.fromUrl(path)))
        case _                           => Response(pages.main(db))
    
    server.task.await()
    
  catch case err: Throwable =>
    println(err.toString+" at "+err.getStackTrace().nn.to(List).mkString("\n"))
