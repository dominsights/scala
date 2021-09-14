
trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Prod(e1: Expr, e2: Expr) extends Expr

def show(e: Expr): String = e match
    case Number(n) => n.toString
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Prod(e1, e2) => 
        def maybeShowParentheses(exp: Expr) = exp match
            case Prod(_,_) => show(exp)
            case Number(_) => show(exp)
            case _ => s"(${show(exp)})"
        maybeShowParentheses(e1) + " * " + maybeShowParentheses(e2)

show(Prod(Sum(Number(2), Number(1)), Number(3)))
show(Sum(Prod(Number(2), Number(1)), Number(3)))

