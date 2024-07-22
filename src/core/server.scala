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

import anticipation.*, filesystemApi.galileiPath
import cataclysm.*
import cellulose.*
import digression.*
import eucalyptus.*
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents}
import gossamer.*
import gesticulate.*
import fulminate.*
import hieroglyph.*, charEncoders.utf8
import parasite.*, threadModels.virtual, orphanDisposal.await
import contingency.*, strategies.throwUnsafely
import rudiments.*
import scintillate.*
import serpentine.*, pathHierarchies.simple
import spectacular.*
import turbulence.*, stdioSources.virtualMachine

import unsafeExceptions.canThrowAny

given Message is Loggable = Log.silent[Message]

@main
def run(classpath: Text): Unit = supervise:
  try
    val db = unsafely:
      val dirs = classpath.cut(t":").filter(_ != t"").map(_.decodeAs[Path].as[Directory])

      val tastyFiles: List[File] =
        dirs.flatMap(_.descendants.filter(_.is[File]).filter(_.name.ends(t".tasty"))).map(_.as[File]).to(List)

      import pathHierarchies.unix
      Amok.inspect(tastyFiles)

    lazy val server: HttpService = HttpServer(8080).listen:
      request.path match
        case % / p"styles" / p"amok.css" => HttpResponse(styles.main)
        case % / p"fonts" / name         => HttpResponse(Content(media"font/ttf", LazyList(data.font(Name(name.render)))))
        case % / p"images" / name        => HttpResponse(Content(media"image/svg+xml", LazyList(data.image(Name(name.render)).bytes)))
        case % / p"info" / path          => HttpResponse(pages.info(db, Identifier.fromUrl(path.render)))
        case _                           => HttpResponse(pages.main(db))

    server.async.await()

  catch case err: Throwable =>
    println(err.toString+" at "+err.getStackTrace().nn.to(List).mkString("\n"))
