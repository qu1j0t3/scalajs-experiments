package tutorial.webapp

import tutorial.webapp.GraphEnumerate.Edge

object VectorData {

  // Parameters for HP 1347A vector display
  val xMax = 2047
  val yMax = 1512

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
