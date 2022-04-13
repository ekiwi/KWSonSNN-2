package neuroproc.systemtests

import chisel3._
import chiseltest._
import neuroproc._

import scala.collection.mutable

class NeuromorphicProcessorDoStepThreadsTester extends NeuromorphicProcessorTester {

  it should "process an image" taggedAs (SlowTest) in {
    test(new NeuromorphicProcessor())
      .withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation))(RunTest { dut =>
        dut.clock.setTimeout(FREQ)

        def receiveByte(byte: UInt): Cmd = {
          // Start bit
          Do {
            dut.io.uartRx.poke(false.B)
          }
          Step(bitDelay)
          // Byte
          for (i <- 0 until 8) {
            dut.io.uartRx.poke(byte(i))
            dut.clock.step(bitDelay)
          }
          // Stop bit
          dut.io.uartRx.poke(true.B)
          dut.clock.step(bitDelay)

          ???
        }

        def transferByte(): Int = {
          var byte = 0
          // Assumes start bit has already been seen
          dut.clock.step(bitDelay)
          // Byte
          for (i <- 0 until 8) {
            byte = (dut.io.uartTx.peek.litToBoolean << i) | byte
            dut.clock.step(bitDelay)
          }
          // Stop bit
          dut.io.uartTx.expect(true.B)
          //dut.clock.step(bitDelay)
          byte
        }

        // Reset inputs
        Do {
          dut.io.uartRx.poke(true.B)
          dut.io.uartTx.expect(true.B)
          dut.reset.poke(true.B)
        }
        Step()
        Do {
          dut.reset.poke(false.B)
          dut.io.uartTx.expect(true.B)
        }

        // Spawn a receiver thread
        var spikes = Array[Int]()
        var receive = true
        val rec = Fork {
          while (receive) {
            if (!dut.io.uartTx.peek.litToBoolean) {
              val s = transferByte()
              if (s < 200)
                spikes = spikes :+ s
              println(s"Received spike ${s}")
            }
            dut.clock.step()
          }
          ???
        }

        Do {
          // Load an image into the accelerator ...
          println("Loading image into accelerator")
          val recv = for (i <- image.indices)
            // Write top byte of index, bottom byte of index, top byte
            // of rate, and bottom byte of rate
            yield Seq(receiveByte((i >> 8).U(8.W)),
              receiveByte((i & 0xff).U(8.W)),
              receiveByte((image(i) >> 8).U(8.W)),
              receiveByte((image(i) & 0xff).U(8.W)))
          print("Done loading image - ")
          recv
        }

        // ... get its response
        Do {
          println("getting accelerator's response")
        }
        Step(FREQ/2)
        Do {
          receive = false
        }
        Join(rec)
        Do {

          println("Response received - comparing results")

          println(spikes.deep.mkString(","))

          assert(spikes.length == results.length, "number of spikes does not match expected")
          assert(spikes.zip(results).map(x => x._1 == x._2).reduce(_ && _), "spikes do not match expected")
        }
      })
  }
}

abstract class Cmd {
  private var next: Option[Cmd] = None
  private def setNext(n: Cmd): Unit = {
    assert(next.isEmpty)
    next = Some(n)
  }
  def step(n: Int = 1): Step = { val s = Step(n) ; setNext(s) ; s }
  def apply(foo: => Unit): Do = { val d = Do(foo) ; setNext(d) ; d }
  def join(thread: => Thread): Join = { val j = Join(thread) ; setNext(j) ; j }
  def fork(thread: => Cmd): Thread = { val t = Fork(thread) ; setNext(t) ; t }
}

// this would eventually be merge into the `test(){}` function
object RunTest {
  def apply[M <: Module](cmd: M => Cmd)(dut: M): Unit = {
    new Interpreter(dut).run(cmd)
  }
}


class Interpreter[M <: Module](dut: M) {
  def run(cmd: M => Cmd): Unit = {

  }

  private val nextCmd = mutable.ArrayBuffer[Cmd]()

  private def run(cmd: Cmd): Unit = cmd match {
    case d: Do =>
      d.foo()


    case join: Join => ???
    case step: Step => ???
    case thread: Thread => ???
    case other => throw new RuntimeException(s"Unknown command: $other")
  }
}

object Do {
  def apply(foo: => Unit): Do = {
    ???
  }
}

class Do(val foo: () => Unit) extends Cmd {
}

object Step {
  def apply(n: Int = 1): Step = new Step(n)
}

class Step(n: Int) extends Cmd {}

object Fork {
  def apply(thread: => Cmd): Thread = ???
}

object Join {
  def apply(thread: => Thread): Join = ???
}

class Join extends Cmd {}

class Thread() extends Cmd {

}

/*

  object Test {
    def receiveByte(byte: UInt) = {
      Do {
        dut.io.uartRx.poke(false.B)
      }
        .step(bitDelay)


      // Byte
      for (i <- 0 until 8) {
        Do {
          dut.io.uartRx.poke(byte(i))
        }
          .step(bitDelay)
      }
      // Stop bit
      Do {
        dut.io.uartRx.poke(true.B)
      }
        .step(bitDelay)
    }


    def transferByte(): Future[Int] = {
      Do {
        var byte = 0
          // Assumes start bit has already been seen
          step(bitDelay)
          // Byte
          .Do(
            for (i <- 0 until 8) {
              yield
              Do {
                byte = (dut.io.uartTx.peek.litToBoolean << i) | byte
              }.
                step(bitDelay)
            }
          )
        // Stop bit
        Do {
          dut.io.uartTx.expect(true.B)
          //dut.clock.step(bitDelay)
          byte
        }
      }
    }
  }


trait Cmd {
  def step(n: Int = 1): Step
}

case class Do(foo: () => Unit, next: Option[Cmd] = None) extends Cmd
case class Step(n: Int = 1, next: Option[Cmd] = None) extends Cmd
 */