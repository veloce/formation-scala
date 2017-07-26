package json

trait Result[+A] {
  def isSuccess = this match {
    case Success(_) => true
    case Failure(_) => false
  }

  def map[U](f: A => U): Result[U]
}

case class Success[+A](value: A) extends Result[A] {
  def map[U](f: A => U): Result[U] = copy(value = f(value))
}

case class Failure(errors: Seq[String]) extends Result[Nothing] {
  def map[U](f: Nothing => U): Result[U] = this
}

// object Result {
//   implicit val applicativeResult: Applicative[Result] = new Applicative[Result] {
//     def pure[A](a: A): Result[A] = Success(a)
//     def map[A, B](m: Result[A], f: A => B): Result[B] = m.map(f)
//     def apply[A, B](mf: Result[A => B], ma: Result[A]): Result[B] = (mf, ma) match {
//       case (Success(f), Success(a)) => Success(f(a))
//       case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
//       case (Failure(e), _) => Failure(e)
//       case (_, Failure(e)) => Failure(e)
//     }
//   }
// }
