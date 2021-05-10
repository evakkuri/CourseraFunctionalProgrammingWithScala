package circuitsimulator

import scala.annotation.tailrec

trait Simulation {
    type Action = () => Unit
    case class Event(time: Int, action: Action)
    private type Agenda = List[Event]
    private var agenda: Agenda = List()

    private var currtime: Int = 0
    def currentTime: Int = currtime

    def afterDelay(delay: Int)(block: => Unit): Unit = {
        val item = Event(currentTime + delay, () => block)
        agenda = insert(agenda, item)
    }

    private def insert(ag: Agenda, item: Event): Agenda = ag match {
        case first :: rest if first.time <= item.time =>
            first :: insert(rest, item)
        case _ =>
            item :: ag
    }

    @tailrec
    private def loop(): Unit = agenda match {
        case first :: rest =>
            agenda = rest
            currtime = first.time
            first.action()
            loop()
        
        case Nil =>
            println("No actions found, stopping.")
    }

    def run(): Unit = {
        afterDelay(0) {
            println("*** simulation started, time: " + currentTime + " ***")
        }
        loop()
    }
}
