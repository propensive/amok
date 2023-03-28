package amok

import rudiments.*
import gossamer.*
import probably.*
import galilei.*, filesystems.unix
import anticipation.*, fileApi.galileiApi

import unsafeExceptions.canThrowAny

object Tests extends Suite(t"Amok Tests"):
  def run(): Unit =
    val files = Unix.parse(t"/home/propensive/.cache/irk/cls/amok/entities").directory(Expect).descendants.filter(_.name.ends(t".tasty")).files
    val docs: Docs = Amok.inspect(files)
    
    test(t"Read object"):
      docs.rootpackage.MyObject
    .assert(_.entity == Entity.Module)
    
    test(t"Read class"):
      docs.rootpackage.MyClass()
    .assert(_.entity == Entity.Class(`abstract` = false))

    test(t"Read abstract class"):
      docs.rootpackage.MyAbstractClass()
    .assert(_.entity == Entity.Class(`abstract` = true))
    
    test(t"Read trait"):
      docs.rootpackage.MyTrait()
    .assert(_.entity == Entity.Trait)
