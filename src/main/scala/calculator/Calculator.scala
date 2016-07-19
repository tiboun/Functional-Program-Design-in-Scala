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
    namedExpressions.map{
      case (varName, signal) => varName -> Signal(eval(signal.apply(), namedExpressions))
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def eval(expr:Expr, referenceUsed:List[String]=Nil):Double= {
      expr match {
        case Literal(v) => v
        case Ref(ref) => {
          if(referenceUsed.contains(ref)){
            Double.NaN
          } else if (references.contains(ref)){
            eval(references.get(ref).get.apply(), ref :: referenceUsed)
          } else {
            Double.NaN
          }
        }
        case Plus(a,b) => eval(a,referenceUsed) + eval(b, referenceUsed)
        case Minus(a,b) => eval(a,referenceUsed) - eval(b, referenceUsed)
        case Times(a,b) => eval(a,referenceUsed) * eval(b, referenceUsed)
        case Divide(a,b) => eval(a,referenceUsed) / eval(b, referenceUsed)
      }
    }
    eval(expr)
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
