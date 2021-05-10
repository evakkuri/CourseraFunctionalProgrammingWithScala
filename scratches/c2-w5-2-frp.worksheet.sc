import scala.util.DynamicVariable

class Signal[T](expr: => T) {
    import Signal._
    private var myExpr: () => T = _
    private var myValue: T = _
    private var observers: Set[Signal[_]] = Set()
    update(expr)

    protected def update(expr: => T): Unit = {
        myExpr = () => expr
        computeValue()
    }

    protected def computeValue(): Unit = {
        val newValue = caller.withValue(this)(myExpr())
        if (myValue != newValue) {
            myValue = newValue
            val obs = observers
            observers = Set()
            obs.foreach(_.computeValue())
        }
    }

    def apply(): T = {
        observers += caller.value
        assert(!caller.value.observers.contains(this), "cyclic signal definition")
        myValue
    }
}

object Signal {
    private val caller = new DynamicVariable[Signal[_]](NoSignal)
    def apply[T](expr: => T) = new Signal(expr)
}

object NoSignal extends Signal[Nothing](???) {
    override protected def computeValue(): Unit = ()
}

class Var[T](expr: => T) extends Signal[T](expr) {
    override def update(expr: => T): Unit = super.update(expr)
}

object Var {
    def apply[T](expr: => T) = new Var(expr)
}

class StackableVariable[T](init: T) {
    private var values: List[T] = List(init)
    def value: T = values.head
    def withValue[R](newValue: T)(op: => R): R = {
        values = newValue :: values
        try op finally values = values.tail
    }
}

class BankAccount {
    val balance = Var(0)

    def currentBalance: Int = balance()

    def deposit(amount: Int): Unit = {
        if (amount >= 0) {
            val b = balance()
            balance() = b + amount
        }
    }

    def withdraw(amount: Int): Unit = {
        if (0 < amount & amount < balance()) {
            val b = balance()
            balance() = b - amount
        } else throw new Error("Invalid amount.") 
    }
}

def consolidated(accts: List[BankAccount]) =
    Signal(accts.map(_.balance()).sum)

val a = new BankAccount()
print(a.balance())
val b = new BankAccount()
val total = consolidated(List(a,b))

a deposit 20
b deposit 30
print(s"Total balance: ${total()}")