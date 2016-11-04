package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
                     namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    var map: Map[String, Signal[Double]] = Map()

    def matchExpr(name: String, expr:Expr, list: List[String]): Boolean = expr match {
      case Literal(v)=> {
        false
      }
      case Ref(n) =>{
        (list contains n) || matchExpr(n,getReferenceExpr(n,namedExpressions),n::list)
      }
      case Plus(a,b) =>{
        matchExpr(name,a,name::list) ||  matchExpr(name,b,name::list)
      }
      case Minus(a,b) => {
        matchExpr(name,a,name::list) || matchExpr(name,b,name::list)
      }
      case Times(a,b) => {
        matchExpr(name,a,name::list) || matchExpr(name,b,name::list)
      }
      case Divide(a,b) =>{
        matchExpr(name,a,name::list) || matchExpr(name,b,name::list)
      }
    }

    for ((name, expr) <- namedExpressions) {
      map += (name -> Signal{
        if(!matchExpr(name, expr(),Nil)) {
          eval(expr(), namedExpressions)
        }
        else {
          Double.NaN
        }
      })
    }

    return map
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Plus(a,b) => eval(a,references) + eval(b,references)
      case Minus(a,b) => eval(a,references) - eval(b,references)
      case Divide(a,b) => eval(a,references) / eval(b,references)
      case Times(a,b) => eval(a,references) * eval(b,references)
      case Ref(name) => eval(getReferenceExpr(name,references),references)
    }
  }

  /** Get the Expr for a referenced variables.
    *  If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String,
                               references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}