package ph.samson.atbp.md2c

import zio.test.Assertion

object AssertionHelpers {

  def inside[T](check: PartialFunction[T, Boolean]): Assertion[T] =
    Assertion.assertion[T]("inside")(t =>
      check.isDefinedAt(t) && check.apply(t)
    )
}

export AssertionHelpers.*
