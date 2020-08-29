package tutorial.webapp

import java.nio.charset.StandardCharsets

import com.fazecast.jSerialComm._

object HpGl {
  // Parameters for HP 1347A vector display
  val xMax = 2047
  val yMax = 1512

  def send(commands: Iterable[HpGlCommand]): Unit = {
    val port = SerialPort.getCommPort("/dev/tty.wchusbserial5d10")
    def writeString(s: String): Unit = {
      val bytes = s.getBytes(StandardCharsets.US_ASCII)
      port.writeBytes(bytes, bytes.length)
      Thread.sleep(10)
    }


    @volatile var t0 = System.currentTimeMillis()

    val readyListener = new SerialPortMessageListener {
      @volatile var ready = false
      override def getMessageDelimiter: Array[Byte] = "Ready.".getBytes(StandardCharsets.US_ASCII)
      override def delimiterIndicatesEndOfMessage(): Boolean = true
      override def getListeningEvents: Int = SerialPort.LISTENING_EVENT_DATA_RECEIVED
      override def serialEvent(event: SerialPortEvent): Unit = {
        println(s"Adapter ready after ${System.currentTimeMillis() - t0} ms")
        ready = true
      }
    }

    port.setComPortParameters(115200, 8, 1, SerialPort.NO_PARITY)
    port.setComPortTimeouts(SerialPort.TIMEOUT_READ_SEMI_BLOCKING, 500, 500)
    assert(port.openPort(), "Port could not be opened");

    try {
      port.addDataListener(readyListener)

      while (!readyListener.ready) {
        Thread.sleep(100)
      }

      port.removeDataListener()

      writeString("\n")
      writeString("++auto 0\n")  // do not try to receive response after message
      writeString("++addr 1\n")  // device address
      writeString("++v 2\n") // 2 = enable status prompt after each command or message
      writeString("\n")

      // Tiny state machine to flush data from port and sync up to '[]'
      // which is the response expected after the empty command.
      var st = 2
      val b = Array[Byte](0)
      // flush everything until the expected '[]'; let a timeout end this phase
      while(st != 0 && port.readBytes(b, 1) == 1) {
        if(b(0) == '['.toByte) {
          st = 1
        } else if (st == 1 && b(0) == ']'.toByte) {
          println("\nSynced")
          st = 0
        } else {
          print(b(0).toChar)
          st = 2 // throw away anything else, but reset state
        }
      }

      println(s"Sending messages")

      (Page :: commands.toList).foreach { cmd =>
        val bytes = Array[Byte](0, 0, 0)
        writeString(cmd.text + ";\n")

        if(port.readBytes(bytes, 3) == 3) {
          if (bytes(0) == '['.toByte && bytes(2) == ']'.toByte) {
            if (bytes(1) != '1'.toByte) {
              throw new Exception(s"Message error after: ${cmd.text}")
            }
          } else {
            throw new Exception(s"Bad status format: ${new String(bytes, StandardCharsets.US_ASCII)}")
          }
        } else {
          throw new Exception("Read timed out?")
        }

      }
      println(s"Sent all messages")
    }
    finally {
      port.closePort()
    }
  }
}
