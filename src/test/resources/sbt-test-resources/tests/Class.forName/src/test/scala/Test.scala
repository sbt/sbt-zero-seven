package test

import org.specs._

object TestSpecification extends Specification
{
  "Class.forName must work in libraries used in tests" in {
     val a: AnyRef = lib.Test.other
     a must notBe(null)
  }
}