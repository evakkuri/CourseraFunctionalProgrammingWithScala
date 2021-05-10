package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator extends CalculatorInterface {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (key, signal) <- namedExpressions
    } yield (key, Signal(eval(signal(), namedExpressions - key)))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case Literal(v) => v
      case Ref(name) => eval(getReferenceExpr(name, references), references)
      case Plus(a, b) => {
        val aEval = eval(a, references)
        val bEval = eval(b, references)
        aEval + bEval
      }
      case Minus(a, b) => {
        val aEval = eval(a, references)
        val bEval = eval(b, references)
        aEval - bEval
      }
      case Times(a, b) => {
        val aEval = eval(a, references)
        val bEval = eval(b, references)
        aEval * bEval
      }
      case Divide(a, b) => {
        val aEval = eval(a, references)
        val bEval = eval(b, references)
        
        try {
          (aEval / bEval)
        } catch {
          case e: java.lang.ArithmeticException => Double.NaN
        }
      }
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
