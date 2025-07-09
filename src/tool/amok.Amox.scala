package amok

import soundness.{is as _, Node as _, *}

object Amox:
  case class Base
              (domain:   Text,
               language: Optional[Text],
               base:     Optional[Text],
               memo:     Optional[Text],
               detail:   Optional[Text],
               entry:    List[Entry])

  object Entry:
    given decoder: Void => CodlDecoder[Entry] = CodlDecoder.derived
    given encoder: Void => CodlEncoder[Entry] = CodlEncoder.derived

  case class Entry
              (name:     Text,
               memo:     Optional[Text],
               detail:   Optional[Text],
               until:    List[Rename],
               hidden:   Optional[Boolean],
               refer:    List[Text],
               entry:    List[Entry])

  case class Rename(version: Text, name: Text)

  def read(file: Path on Linux)(using Stdio)
  : Base raises CodlError raises CodlReadError raises IoError raises StreamError =

      file.open(Codl.read[Base](_))
