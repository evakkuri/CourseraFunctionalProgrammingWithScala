trait Subscriber {
    def handler(publisher: Publisher)
}

trait Publisher {
    private var subscribers: Set[Subscriber] = Set()

    def subscribe(subscriber: Subscriber): Unit =
        subscribers += subscriber

    def unsubscribe(subscriber: Subscriber): Unit =
        subscribers -= subscriber

    def publish(): Unit =
        subscribers.foreach(_.handler(this))
}

class BankAccount extends Publisher {
    private var balance = 0

    def currentBalance: Int = balance

    def deposit(amount: Int): Unit = {
        if (amount >= 0) balance += amount
        publish()
    }

    def withdraw(amount: Int): Unit = {
        if (0 < amount & amount < balance) {
            balance -= amount
            publish()
        } else throw new Error("Invalid amount.") 
    }
}

class Consolidator(observed: List[BankAccount]) extends Subscriber {
    observed.foreach(_.subscribe(this))

    private var total: Int = _
    compute()

    def compute() =
        total = observed.map(_.currentBalance).sum

    def handler(publisher: Publisher): Unit = compute()

    def totalBalance = total
}

val a = new BankAccount
val b = new BankAccount

val cons = new Consolidator(List(a, b))
cons.totalBalance

a.deposit(50)
cons.totalBalance

b.deposit(30)
cons.totalBalance