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