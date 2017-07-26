
package col

sealed trait Col[+A] {

  def size: Int = this match {
    case Empty() => 0
    case OneAnd(a, tail) => 1 + tail.size
  }

  def concat[B >: A](as: Col[B]): Col[B] = this match {
    case Empty() => as
    case OneAnd(a, tail) => OneAnd(a, tail.concat(as))
  }

  def reverse: Col[A] = this match {
    case e: Empty[A] => e
    case OneAnd(a, tail) => tail.reverse.concat(OneAnd(a, Empty()))
  }

  def map[B](f: A => B): Col[B] = this match {
    case Empty() => Empty[B]()
    case OneAnd(a, tail) => OneAnd(f(a), tail.map(f))
  }

  def filter(p: A => Boolean): Col[A] = this match {
    case e: Empty[A] => e
    case OneAnd(a, tail) => if (p(a)) OneAnd(a, tail.filter(p)) else tail.filter(p)
  }

  def forall(p: A => Boolean): Boolean = this match {
    case Empty() => true
    case OneAnd(a, tail) => if (p(a)) true else tail.forall(p)
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty() => false
    case OneAnd(a, tail) => if (p(a)) true else tail.forall(p)
  }

  def fold[B](b: B)(f: (A, B) => B): B = this match {
    case Empty() => b
    case OneAnd(a, tail) => tail.fold(f(a, b))(f)
  }
}

case class Empty[A]() extends Col[A]
case class OneAnd[A](a: A, tail: Col[A]) extends Col[A]
