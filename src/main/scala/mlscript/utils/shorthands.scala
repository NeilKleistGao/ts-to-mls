package mlscript.utils

import scala.annotation.showAsInfix

// from https://github.com/hkust-taco/mlscript/blob/mlscript/shared/src/main/scala/mlscript/utils/shorthands.scala#L41

object shorthands {
  type \/[+A, +B] = Either[A, B]
  val \/ : Either.type = Either

  @showAsInfix
  type -> [+A,+B] = (A,B)
  object -> {
    def unapply[A, B](ab: (A, B)): Some[(A, B)] = Some(ab)
  }
}