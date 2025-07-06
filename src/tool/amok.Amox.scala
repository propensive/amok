package amok

import scala.tasty.*, inspector.*
import scala.quoted.*
import scala.collection.mutable as scm

import soundness.{is as _, Node as _, *}

object Amox:
  case class Base(base: Text, memo: Optional[Text], detail: Optional[Text], entry: List[Entry])

  object Entry:
    given decoder: Void => CodlDecoder[Entry] = CodlDecoder.derived
    given encoder: Void => CodlEncoder[Entry] = CodlEncoder.derived

  case class Entry
              (name:   Text,
               memo:   Optional[Text],
               detail: Optional[Text],
               hidden: Optional[Boolean],
               refer:  List[Text],
               entry:  List[Entry])

  def read(file: Path on Linux)(using Stdio)
  : Base raises CodlError raises CodlReadError raises IoError raises StreamError =

      file.open(Codl.read[Base](_))
