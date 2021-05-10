val testMap = Map("a" -> 1, "b" -> 2)
val f = (value: Int) => value * 2

val doubleSalary = (x: Int) => x * 2
testMap.mapValues((value: Int) => value * 2)

testMap.get("a")

testMap - "a"

/*
def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
*/