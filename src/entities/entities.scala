package rootpackage

object MyObject

class MyClass

object MyClassWithCompanion
class MyClassWithCompanion

val topLevelVal: Int = 0
var topLevelVar: Int = 0
def topLevelDef: Int = 0
inline def topLevelInlineDef: Int = 0
transparent inline def topLevelTransparentInlineDef: Int = 0

trait MyTrait

abstract class MyAbstractClass()

open class MyOpenClass()


package innerpackage:
  val innerpackageVal: Int = 0


object Nested:
  val myVal: Int = 0
  lazy val myLazyVal: Int = 0
  var myVar: Int = 0
  def myDef: Int = 0

  inline def myInlineDef: Int = 0

  transparent inline def myTransparentInlineDef: Int = 0

  erased val myErasedVal: Int = compiletime.erasedValue