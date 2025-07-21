package amok

import scala.tasty.*, inspector.*
import scala.quoted.*

import soundness.{is as _, Node as _, *}

enum Visibility:
  case Private, Protected, Public
