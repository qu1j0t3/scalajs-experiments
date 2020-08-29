package tutorial.webapp

import java.io._

import tutorial.webapp.GraphEnumerate.Edge

object VectorData {
  import HpGl._

  // GPIB adapter setup to communicate interactively with HP:
  // ++v 1        ; interactive mode
  // ++addr 1     ; device address
  // ++auto 0     ; don't wait for a response after each message
  // Configure a newline tx delay of 1 milliseconds
  // (if communication is not clean, increase this to 5ms or more)
  // This assumes 1 HPGL command per line/message.

  def run(): Unit = {
    val order = 4
    val gs = GraphEnumerate.graphs(order)

    val xCentre = xMax/2
    val yCentre = yMax/2
    // Aim for a squarish layout
    val cols = Math.ceil(Math.sqrt(gs.length))
    val rows = (gs.length + cols - 1)/cols
    val graphSpacing = yMax / rows; // Height of screen is less than width
    val graphRadius = graphSpacing * 0.3

    val commands = gs.zipWithIndex.flatMap { case (g,idx) =>
      // graph nodes are labelled 1..order

      val cx = xCentre + ((idx % cols) - (cols-1)/2.0) * graphSpacing
      val cy = yCentre - (Math.floor(idx / cols) - (rows-1)/2.0) * graphSpacing

      def pos(i: Int): (Double,Double) = {
        val a = 2*Math.PI*i/order
        (cx + graphRadius*Math.cos(a),
         cy + graphRadius*Math.sin(a))
      }

      val edges = g.toList.flatMap {
        case Edge(u, v) =>
          List(
            PenUp, PlotAbs(pos(u)),
            PenDown, PlotAbs(pos(v))
          )
      }

      val connectedNodes = g.toList.flatMap { case Edge(u, v) => List(u, v) }.toSet

      val nodes = (1 to order).flatMap { node =>
        // Don't draw nodes that are part of an edge
        if (!connectedNodes.contains(node)) {
          val (x, y) = pos(node)
          List(
            PenUp, PlotAbs((x - 4, y - 4)),
            PenDown, PlotRel((4, 4)),
            PenUp, PlotRel((-4, 0)),
            PenDown, PlotRel((4, -4))
          )
        } else {
          Nil
        }
      }.toList

      List(PenUp, PlotAbs((cx-graphRadius, cy-graphRadius-50)), Label(g.size.toString)) ++ edges ++ nodes
    }

    HpGl.send(commands)
  }

}
