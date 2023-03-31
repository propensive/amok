package amok

import turbulence.*, characterEncodings.utf8
import galilei.*
import rudiments.*
import serpentine.*
import deviation.*

object data:
  private val classpath = Classpath()
  def font(name: Text): Bytes = unsafely((classpath / p"amok" / p"fonts" / name).read[Bytes])
  def image(name: Text): Text = unsafely((classpath / p"amok" / p"images" / name).read[Text])
