package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay){output.setSignal(a1Sig || a2Sig)}
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1InvToAnd = new Wire
    inverter(a1, a1InvToAnd)
    val a2InvToAnd = new Wire
    inverter(a2, a2InvToAnd)
    
    val andToInv = new Wire
    andGate(a1InvToAnd, a2InvToAnd, andToInv)

    inverter(andToInv, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]): Unit = c match {
    case Nil => connector(in, out(0))
    case cHead :: cTail => {

      val out1, out2 = new Wire

      val controlInverted = new Wire
      inverter(cHead, controlInverted)

      andGate(in, controlInverted, out1)
      andGate(in, cHead, out2)

      val outHalfs = out splitAt (out.length / 2)
      demux(out1, cTail, outHalfs._2)
      demux(out2, cTail, outHalfs._1)

      // in cont = out1 out2
      //0 0 = 0 0
      //0 1 = 0 0
      //1 0 = 1 0
      //1 1 = 0 1

    }

  }
  
  def connector(in: Wire, out: Wire) = {
    def transmit() {
      val inSig = in.getSignal
      afterDelay(0)(out.setSignal(inSig))
    }
    in addAction transmit
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
