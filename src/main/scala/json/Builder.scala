package json

trait Builder[M[_]] {

  def apply[A, B](ma: M[A], mb: => M[B]): M[(A, B)]

}

final class BuilderOps[M[_], A](ma: M[A])(implicit builder: Builder[M]) {

  def and[B](mb: => M[B]): M[(A, B)] = builder(ma, mb)

}

object BuilderOps {
  implicit def toFunctionalBuilderOps[M[_], A](a: M[A])(implicit fcb: Builder[M]) = new BuilderOps[M, A](a)(fcb)
}
