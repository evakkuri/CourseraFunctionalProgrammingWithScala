implicit val n1: Int = 42
def f1(implicit x: Int) = x
println(f1)

/*
implicit val n2: Int = 42
def f2(implicit x: Int) = x
println(f2(0)) // This returns 0. Because the parameter is provided explicitly?
*/