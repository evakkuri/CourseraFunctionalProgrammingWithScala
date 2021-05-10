/*
 * Example 1: Basic threading
 */

class HelloThread extends Thread {
    override def run(): Unit = {
        println("Hello ")
        println("world!")
    }
}

def main(): Unit = {
    val t = new HelloThread
    val s = new HelloThread
    t.start()
    s.start()
    t.join()
    s.join()
}

// At times the threads finish sequentially, other times interleaves
main()

/*
 * Example 2: Atomicity, unique ID addition
 */

// The below example is not atomic, multiple threads can access the same value
// causing duplicates
private var uidCount = 0L

def getUniqueId(): Long = {
    uidCount += 1
    uidCount
}

def startThread() = {
    val t = new Thread {
        override def run(): Unit = {
            val uids = for (i <- 0 until 10) yield getUniqueId()
            println(uids)
        }
    }
    t.start()
    t
}

val t1 = startThread()
val t2 = startThread()
t1.join()
t2.join()

// Instead we can use the "synchronized" construct to enforce atomicity.
// This way, Scala monitors that at most 1 thread is executing the code at any
// given time. Below, value x functions as a global monitor variable on the
// atomic ID's.
private val x = new AnyRef{}
private var uidCountAtomic = 0L
def getUniqueIdAtomic(): Long = x.synchronized {
    uidCount += 1
    uidCount
}

/*
 * Example 3: Nested synchronized blocks
 */

// As example, an Account class that requires monitor on both the current 
// account and the target account to finalize
class Account (private var amount: Int = 0) {
    def transfer(target: Account, n: Int) =
    this.synchronized {
        target.synchronized {
            this.amount -= n
            target.amount += n
        }
    }
}

// We again define a method for starting a new transfer thread
def startAccountThread(a: Account, b: Account, n: Int): Thread = {
    val t = new Thread {
        override def run() {
            for (i <- 0 until n) {
                a.transfer(b, 1)
            }
        }
    }
    t.start()
    t
}

val a1 = new Account(500)
val a2 = new Account(700)

// The current implementation is susceptible to deadlocks. For example, if you
// were to define two opposite threads as follows, this would cause a deadlock
// and the program will never finish
val t3 = startAccountThread(a1, a2, 150)
//val t4 = startAccountThread(a2, a1, 150)
t3.join()
//t4.join()

// We can use the previously defined atomic getUniqueId() to help. We set each
// Account instance with a unique ID, and use that to ensure that the lockings
// are always attempted in the same order. This in turn ensures that one
// operation will be allowed to finish before another.
class NoDeadlockAccount (private var amount: Int = 0) {
    val uid = getUniqueIdAtomic()

    private def lockAndTransfer(target: NoDeadlockAccount, n: Int) =
        this.synchronized {
            target.synchronized {
                this.amount -= n
                target.amount += n
            }
        }

    def transfer(target: NoDeadlockAccount, n: Int) =
        if (this.uid < target.uid) this.lockAndTransfer(target, n)
        else target.lockAndTransfer(this, -n)
}

// We again define a method for starting a new transfer thread, this time with
// with no-deadlock Account objects
def startNoDeadlockAccountThread(a: NoDeadlockAccount, b: NoDeadlockAccount, n: Int): Thread = {
    val t = new Thread {
        override def run() {
            for (i <- 0 until n) {
                a.transfer(b, 1)
            }
        }
    }
    t.start()
    t
}

val noDLa1 = new NoDeadlockAccount(500)
val noDLa2 = new NoDeadlockAccount(700)

val t5 = startNoDeadlockAccountThread(noDLa1, noDLa2, 150)
val t6 = startNoDeadlockAccountThread(noDLa2, noDLa1, 150)
t5.join()
t6.join()