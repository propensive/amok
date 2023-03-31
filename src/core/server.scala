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
import deviation.*
import galilei.*, filesystems.unix
import gossamer.*
import anticipation.*, fileApi.galileiApi
import scintillate.*
import serpentine.*
import parasitism.*, monitors.global
import rudiments.*
import eucalyptus.*, logging.stdout
import turbulence.*, characterEncodings.utf8, basicIo.jvm

import unsafeExceptions.canThrowAny

@main
def run(): Unit =
  try
    val docs = unsafely:
      val dirs = List(Unix.parse(t"/home/propensive/work/amok/out").directory(Expect))
      val tastyFiles = dirs.flatMap(_.descendants.filter(_.name.ends(t".tasty")).files)
      Amok.inspect(tastyFiles)
    
    def rewrite(node: CodlNode): CodlNode =
      node.copy(data = node.data.mm { data => data.copy(children = data.children.map(rewrite)) }).promote(1)

    val codec = summon[Codec[Docs]]
    val codl = CodlDoc(IArray.from(codec.serialize(docs).flatten).map(rewrite), codec.schema, 0)

    lazy val server: ActiveServer = HttpServer(8080).listen:
      request.path match
        case ^ / t"styles" / t"amok.css" => Response(styles.main)
        case ^ / t"fonts" / name         => Response(Ttf(data.font(name)))
        case ^ / t"images" / name        => Response(Svg(data.image(name)))
        case ^ / t"info" / name          => Response(pages.info(name))
        case _                           => Response(pages.main(docs))
    
    server.task.await()
    
  catch case err: Throwable =>
    println(err.toString+" at "+err.getStackTrace().nn.to(List).mkString("\n"))