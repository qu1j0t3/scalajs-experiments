package tutorial.webapp

import java.nio.charset.StandardCharsets

import com.fazecast.jSerialComm.SerialPort

// command:
// instruction   parameter field   terminator
// parameter field:
// - integer -32768..+32767 or -16384..16383
// - decimal +/-127.999
// - label fields: any combination of text,
//   numeric expressions, or string variables
// * when using the label instruction LB,
//   the terminator used must be a binary 3 (ETX)
// * certain instructions such as PA or PR may have
//   multiple or optional parameters. These parameters
//   must be separated by commas


sealed trait HpGlPen {
  def number: Int
}
case object BeamOff extends HpGlPen { def number = 0 }
case object FullBrightness extends HpGlPen { def number = 1 }
case object HalfBrightness extends HpGlPen { def number = 2 }
case object DimBrightness extends HpGlPen { def number = 3 }

sealed trait HpGlCommand {
  def n(x: Double): String = Math.round(x).toString
  def ns(ps: Iterable[(Double,Double)]): String =
    ps.map{ case (x,y) => s"${n(x)},${n(y)}" }.mkString(",")

  def text: String
}
case object Page extends HpGlCommand {
  def text = "PG"
}
case object PenUp extends HpGlCommand {
  def text = "PU"
}
case object PenDown extends HpGlCommand {
  def text = "PD"
}
// Both X- and Y-parameters in integer display units having values between
// 0 to 2047 for the X parameter and 0 to 1512 for the Y parameter.
case class PlotAbs(ps: (Double,Double)*) extends HpGlCommand {
  def text = s"PA ${ns(ps)}"
}
// A positive value moves the beam up and to the right,
// and a negative value moves the beam down and to the left.
case class PlotRel(ps: (Double,Double)*) extends HpGlCommand {
  def text = s"PR ${ns(ps)}"
}
case class SelectPen(pen: HpGlPen) extends HpGlCommand {
  def text = s"SP ${pen.number}"
}
