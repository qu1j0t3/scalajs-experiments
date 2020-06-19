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
  case class PlotAbs(ps: (Double,Double)*) extends HpGlCommand {
    def text = s"PA ${ns(ps)}"
  }
  case class PlotRel(ps: (Double,Double)*) extends HpGlCommand {
    def text = s"PR ${ns(ps)}"
  }
  case class SelectPen(pen: HpGlPen) extends HpGlCommand {
    def text = s"SP ${pen.number}"
  }

  def run(): Unit = {
    val order = 3
    val gs = GraphEnumerate.graphs(order)

    val graphSpacing = 50;
    val graphRadius = 15;
    val cols = Math.ceil(Math.sqrt(gs.length))

    gs.zipWithIndex.flatMap { case (g,idx) =>
      // graph nodes are labelled 1..order

      def pos(i: Int): (Double,Double) = {
        val cx = ((idx % cols)+1) * graphSpacing
        val cy = (Math.floor(idx / cols)+1) * graphSpacing
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
