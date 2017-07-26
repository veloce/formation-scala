package bool

object Lazy {
  sealed trait Bool {
    def and(b: => Bool): Bool = ifThenElse(this)(b, False)
    def or(b: => Bool): Bool = ifThenElse(this)(True, b)
  }

  case object True extends Bool
  case object False extends Bool

  def ifThenElse[T](cond: Bool)(a: => T, b: => T): T = cond match {
    case True => a
    case False => b
  }
}
