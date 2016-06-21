package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions map { case (key, expression) => key -> Signal { eval(expression(), namedExpressions) } }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def eval(expr: Expr, references: Map[String, Signal[Expr]], stack: Set[String]): Double =
      expr match {
        case Literal(v)   => v
        case Plus(a, b)   => eval(a, references, stack) + eval(b, references, stack)
        case Minus(a, b)  => eval(a, references, stack) - eval(b, references, stack)
        case Times(a, b)  => eval(a, references, stack) * eval(b, references, stack)
        case Divide(a, b) => eval(a, references, stack) / eval(b, references, stack)
        case Ref(name)    =>
          if(stack contains name) Double.NaN
          else eval(getReferenceExpr(name, references), references, stack + name)
      }

    eval(expr, references, Set())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) =
    references.get(name).fold[Expr] { Literal(Double.NaN) } { exprSignal => exprSignal() }

}
