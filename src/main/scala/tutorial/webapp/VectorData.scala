package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import tutorial.webapp.GraphEnumerate.{Edge, MyApiImpl, appendPre, graphs}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

object VectorData {

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

  // Parameters for HP 1347A vector display
  val xMax = 2047
  val yMax = 1512

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

  def run(): Unit = {
    val order = 4
    val gs = GraphEnumerate.graphs(order)

    val xCentre = xMax/2
    val yCentre = yMax/2
    // Aim for a squarish layout
    val cols = Math.ceil(Math.sqrt(gs.length))
    val rows = (gs.length + cols - 1)/cols
    val graphSpacing = yMax / (rows+2); // Height of screen is less than width
    val graphRadius = graphSpacing * 0.3

    gs.zipWithIndex.flatMap { case (g,idx) =>
      // graph nodes are labelled 1..order

      def pos(i: Int): (Double,Double) = {
        val cx = xCentre + ((idx % cols) - (cols-1)/2.0) * graphSpacing
        val cy = yCentre + (Math.floor(idx / cols) - (rows-1)/2.0) * graphSpacing
        val a = 2*Math.PI*i/order
        (cx + graphRadius*Math.cos(a),
         cy + graphRadius*Math.sin(a))
      }

      val edges = g.toList.flatMap {
        case Edge(u, v) =>
          List(
            PenUp,
            PlotAbs(pos(u)),
            PenDown,
            PlotAbs(pos(v))
          )
      }

      val nodes = (1 to order).flatMap { node =>
        val (x,y) = pos(node)
        List(
          PenUp,
          PlotAbs( (x-4,y) ),
          PenDown,
          PlotRel( (4,-4), (4,4), (-4,4), (-4,-4) )
        )
      }.toList

      SelectPen(FullBrightness) :: (edges ++ (SelectPen(HalfBrightness) :: nodes))

    }.foreach(c => println(c.text))
  }

}
