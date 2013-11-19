package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  test("orGate") {

    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "0 | 0 = 0")

    in1.setSignal(true)
    in2.setSignal(false)
    run
    assert(out.getSignal === true, "1 | 0 = 1")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "0 | 1 = 1")

    in1.setSignal(true)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "1 | 1 = 1")

  }

  test("orGate2") {

    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "0 | 0 = 0")

    in1.setSignal(true)
    in2.setSignal(false)
    run
    assert(out.getSignal === true, "1 | 0 = 1")

    in1.setSignal(false)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "0 | 1 = 1")

    in1.setSignal(true)
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "1 | 1 = 1")

  }

  test("demux 0 cont") {

    val in, out = new Wire
    demux(in, List.empty, List(out))

    in.setSignal(false)
    run
    assert(out.getSignal === false)

    in.setSignal(true)
    run
    assert(out.getSignal === true)

  }

  test("demux 1 cont") {

    val in, cont, out1, out2 = new Wire
    demux(in, List(cont), List(out2, out1))

    in.setSignal(false)
    cont.setSignal(false)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)

    in.setSignal(false)
    cont.setSignal(true)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)

    in.setSignal(true)
    cont.setSignal(false)
    run
    assert(out1.getSignal === true)
    assert(out2.getSignal === false)

    in.setSignal(true)
    cont.setSignal(true)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === true)

  }

  test("demux 2 cont") {

    val in, cont1, cont2, out1, out2, out3, out4 = new Wire
    demux(in, List(cont2, cont1), List(out4, out3, out2, out1))

    in.setSignal(false)
    cont1.setSignal(true)
    cont2.setSignal(true)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)
    assert(out4.getSignal === false)

    in.setSignal(true)
    cont1.setSignal(false)
    cont2.setSignal(false)
    run
    assert(out1.getSignal === true)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)
    assert(out4.getSignal === false)

    in.setSignal(true)
    cont1.setSignal(true)
    cont2.setSignal(false)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === true)
    assert(out3.getSignal === false)
    assert(out4.getSignal === false)

    in.setSignal(true)
    cont1.setSignal(false)
    cont2.setSignal(true)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === true)
    assert(out4.getSignal === false)

    in.setSignal(true)
    cont1.setSignal(true)
    cont2.setSignal(true)
    run
    assert(out1.getSignal === false)
    assert(out2.getSignal === false)
    assert(out3.getSignal === false)
    assert(out4.getSignal === true)

  }

}
