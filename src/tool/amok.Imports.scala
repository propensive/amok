package amok

import soundness.*

case class Imports(indexes: Set[Index]):
  def has(index: Index): Boolean = indexes.contains(index)
