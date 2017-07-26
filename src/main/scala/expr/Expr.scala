package expr

sealed trait Expr

case class Number(n: Int) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Multiply(l: Expr, r: Expr) extends Expr

object Expr {
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Add(lhs, rhs) => eval(lhs) + eval(rhs)
    case Multiply(lhs, rhs) => eval(lhs) * eval(rhs)
  }
}


trait ObjExpr {
  def eval(): Int
}

class ObjNumber(n: Int) extends ObjExpr {
  def eval(): Int = n
}

class ObjAdd(lhs: ObjExpr, rhs: ObjExpr) extends ObjExpr {
  def eval(): Int = lhs.eval() + rhs.eval()
}
