package bool

import org.scalatest._

import Lazy._

class LazySpec extends FlatSpec with Matchers {

  "Bool" should "and" in {
    True and True shouldEqual True
    True and False shouldEqual False
    False and True shouldEqual False
    False and False shouldEqual False
  }

  it should "or" in {
    True or True shouldEqual True
    True or False shouldEqual True
    False or True shouldEqual True
    False or False shouldEqual False
  }

}
