package tutorial.webapp

import org.scalajs.dom
import org.scalajs.dom.document
import tutorial.webapp.GraphEnumerate.{Edge, MyApiImpl, appendPre, graphs}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global

object VectorData {

  sealed trait VCommand {
    def n(x: Double): String = Math.round(x).toString
    def ns(ps: List[(Double,Double)]): String =
      ps.map{ case (x,y) => s"${n(x)},${n(y)}" }.mkString(",")

    def text: String
  }
  case object PenUp extends VCommand {
    def text = "PU"
  }
  case object PenDown extends VCommand {
    def text = "PD"
  }
  case class PlotAbs(pt: (Double,Double), more: List[(Double,Double)] = Nil) extends VCommand {
    def text = s"PA ${ns(pt :: more)}"
  }
  case class PlotRel(pt: (Double,Double), more: List[(Double,Double)] = Nil) extends VCommand {
    def text = s"PR ${ns(pt :: more)}"
  }

  def run(): Unit = {
    val order = 3
    val gs = GraphEnumerate.graphs(order)

    val graphSpacing = 50;
    val graphRadius = 15;
    val cols = Math.ceil(Math.sqrt(gs.length))

    gs.zipWithIndex.flatMap { case (g,idx) =>
      // notionally the graph has nodes labelled 1..order

      def pos(i: Int): (Double,Double) = {
        val cx = ((idx % cols)+1) * graphSpacing
        val cy = (Math.floor(idx / cols)+1) * graphSpacing
        val a = 2*Math.PI*i/order
        (cx + graphRadius*Math.cos(a),
         cy + graphRadius*Math.sin(a))
      }

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

      g.toList.flatMap {
        case Edge(u, v) =>
          List(
            PenUp,
            PlotAbs(pos(u)),
            PenDown,
            PlotAbs(pos(v))
          )
      }
    }.foreach(c => println(c.text))
  }

}
