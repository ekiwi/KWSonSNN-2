package neuroproc.systemtests

import neuroproc._
import chisel3._
import chiseltest._
import org.scalatest._

import java.io.{FileNotFoundException, IOException}
import scala.collection.mutable

class NeuromorphicProcessorTester extends FlatSpec with ChiselScalatestTester {
  behavior of "Neuromorphic Processor"

  val bitDelay = FREQ / BAUDRATE + 1

  def fetch(file: String) = {
    val src = scala.io.Source.fromFile(file)
    val lines = try {
      src.mkString
    } catch {
      case e: FileNotFoundException => {
        println("Incorrect path to image file")
        ""
      }
      case e: IOException => {
        println("Cannot open image file")
        ""
      }
    } finally {
      src.close
    }
    lines.split(",").map(_.toInt)
  }

  if (!RANKORDERENC) {
    it should "process an image" taggedAs(SlowTest) in {
      val annos = Seq(
        VerilatorBackendAnnotation,
        chiseltest.internal.NoThreadingAnnotation,
      )
      test(new NeuromorphicProcessor()).withAnnotations(annos) {
        dut =>
          dut.clock.setTimeout(FREQ)
        
          // Reference image and results
          val image = fetch("./src/test/scala/neuroproc/systemtests/image.txt")
          val results = if (USEROUNDEDWGHTS) 
            fetch("./src/test/scala/neuroproc/systemtests/results_round.txt")
          else
            fetch("./src/test/scala/neuroproc/systemtests/results_toInt.txt")

          // Reset inputs
          dut.io.uartRx.poke(true.B)
          dut.io.uartTx.expect(true.B)
          dut.reset.poke(true.B)
          dut.clock.step()
          dut.reset.poke(false.B)
          dut.io.uartTx.expect(true.B)

          // Spawn a receiver thread
          val spikeRxThread = new SpikeReceiver(bitDelay)

          // Load an image into the accelerator ...
          val bytes = image.indices.flatMap { i =>
            Seq((i >> 8) & 0xff, i & 0xff, (image(i) >> 8) & 0xff, image(i) & 0xff)
          }
          val txThread = new UartTxThread(bitDelay, bytes)
          def step(): Unit = {
            spikeRxThread.step(dut)
            txThread.step(dut)
            dut.clock.step()
          }

          println("Loading image into accelerator")
          while(!txThread.done) {
            step()
          }
          print("Done loading image - ")
        
          // ... get its response
          println("getting accelerator's response")
          (0 until (FREQ/2)). foreach { ii =>
            step()
          }
          // join
          while(!spikeRxThread.done) {
            step()
          }
        
          println("Response received - comparing results")
          val spikes = spikeRxThread.spikes
          assert(spikes.length == results.length, "number of spikes does not match expected")
          assert(spikes.zip(results).map(x => x._1 == x._2).reduce(_ && _), "spikes do not match expected")
      }
    }
  }
}

trait IsThread {
  def done: Boolean
  def step(dut: NeuromorphicProcessor): Unit
}

class SpikeReceiver(bitDelay: Int) extends IsThread {
  private val rx = new UartRxThread(bitDelay)
  var spikes = Array[Int]()
  def done: Boolean = rx.done
  def step(dut: NeuromorphicProcessor): Unit = {
    rx.step(dut)
    rx.get() match {
      case Some(s) =>
        if (s < 200)
          spikes = spikes :+ s
        println(s"Received spike ${s}")
      case None =>
    }
  }
}

class UartTxThread(bitDelay: Int, toSend: Seq[Int]) extends IsThread {
  private var state = 0;
  private var delay_count = 0;
  private var byte = 0;
  private val bytes = mutable.Queue[Int]()
  bytes ++= toSend

  def done: Boolean = bytes.isEmpty && delay_count == 0 && state == 0
  def step(dut: NeuromorphicProcessor): Unit = {
    if (delay_count > 0) {
      delay_count -= 1
    } else {
      state = if (state == 0) {
        if(bytes.nonEmpty) {
          byte = bytes.dequeue()
          // Start bit
          dut.io.uartRx.poke(false.B)
          delay_count = bitDelay - 1
          1
        } else {
          0
        }
      } else if(state >= 1 && state < 9) {
        val bit = ((byte >> (state - 1)) & 1) == 1
        dut.io.uartRx.poke(bit)
        delay_count = bitDelay - 1
        state + 1
      } else {
        assert(state == 9)
        // Stop bit
        dut.io.uartRx.poke(true.B)
        delay_count = bitDelay - 1
        0
      }
    }
  }
}


class UartRxThread(bitDelay: Int) extends IsThread {
  private var state = 0;
  private var delay_count = 0;
  private var byte = 0;
  private val bytes = mutable.Queue[Int]()
  def get(): Option[Int] = if(bytes.isEmpty) { None } else { Some(bytes.dequeue()) }

  def done: Boolean = bytes.isEmpty && delay_count == 0 && state == 0
  def step(dut: NeuromorphicProcessor): Unit = {
    if (delay_count > 0) {
      delay_count -= 1
    } else {
      state = if (state == 0) {
        if (!dut.io.uartTx.peekBoolean()) {
          delay_count = bitDelay - 1
          byte = 0
          1
        } else {
          0
        }
      } else if(state >= 1 && state < 9) {
        byte = (dut.io.uartTx.peek.litToBoolean << (state - 1)) | byte
        delay_count = bitDelay - 1
        state + 1
      } else {
        assert(state == 9)
        bytes.enqueue(byte)
        dut.io.uartTx.expect(true)
        0
      }
    }
  }
}


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