package col

import org.scalatest._

class ColSpec extends FlatSpec with Matchers {

  val col1 = OneAnd(1, OneAnd(2, OneAnd(3, OneAnd(4, Empty()))))
  val col2 = OneAnd(5, OneAnd(6, OneAnd(7, Empty())))

  "Col" should "size" in {
    col1.size shouldEqual(4)
  }

  it should "concat" in {
    col1.concat(col2) shouldEqual OneAnd(1, OneAnd(2, OneAnd(3, OneAnd(4, OneAnd(5, OneAnd(6, OneAnd(7, Empty())))))))
  }

  it should "reverse" in {
    col1.reverse shouldEqual OneAnd(4, OneAnd(3, OneAnd(2, OneAnd(1, Empty()))))
  }

  it should "map" in {
    col1.map(x => x*x) shouldEqual OneAnd(1, OneAnd(4, OneAnd(9, OneAnd(16, Empty()))))
  }

  it should "filter" in {
    col1.filter(_ % 2 == 0) shouldEqual OneAnd(2, OneAnd(4, Empty()))
  }

  it should "forall" in {
    Empty[Int]().forall(_ < 0) shouldEqual true
    col1.forall(_ > 0) shouldEqual true
  }

  it should "exist" in {
    Empty[Int]().exists(_ == 3) shouldEqual false
    col1.exists(_ == 3) shouldEqual true
  }

  it should "fold" in {
    col1.fold(0)(_ + _) shouldEqual 10
  }

}
