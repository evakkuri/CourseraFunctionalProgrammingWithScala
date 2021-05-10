package circuitsimulator

trait Gates extends Simulation {

    class Wire {
        private var sigVal = false
        private var actions: List[Action] = List()

        def getSignal: Boolean = sigVal

        def setSignal(s: Boolean): Unit = 
            if (s != sigVal) {
                sigVal = s
                actions foreach (_())
            }

        def addAction(a: Action): Unit = {
            actions = a :: actions
            a()
        }
    }
    
    def InverterDelay: Int
    def inverter(input: Wire, output: Wire): Unit = {
        def invertAction(): Unit = {
            val inputSig = input.getSignal
            afterDelay(InverterDelay) { output setSignal !inputSig}
        }
        input addAction invertAction
    }

    def AndGateDelay: Int
    def andGate(input1: Wire, input2: Wire, output: Wire): Unit = {
        def andAction(): Unit = {
            val inputSig1 = input1.getSignal
            val inputSig2 = input2.getSignal
            afterDelay(AndGateDelay) {
                output setSignal (inputSig1 & inputSig2)
            }
        }
        input1 addAction andAction
        input2 addAction andAction
    }

    def OrGateDelay: Int
    def orGate(input1: Wire, input2: Wire, output: Wire): Unit = {
        def orAction(): Unit = {
            val inputSig1 = input1.getSignal
            val inputSig2 = input2.getSignal
            afterDelay(OrGateDelay) {
                output setSignal (inputSig1 | inputSig2)
            }
        }
        input1 addAction orAction
        input2 addAction orAction
    }

    def probe(name: String, wire: Wire): Unit = {
        def probeAction(): Unit = {
            println(s"$name $currentTime value = ${wire.getSignal}")
        }
        wire addAction probeAction
    }
}
