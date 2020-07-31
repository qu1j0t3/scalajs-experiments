package tutorial.webapp

object FractalDemo {
  import HpGl._

  def run(): Unit = {

    def cross(x: Double, y: Double, xb: Double, yb: Double, radius: Double, depth: Int): List[HpGlCommand] = {
      if (depth > 0) {
        val d = depth - 1
        val r = radius * 0.6
        val rr = radius * 0.4

        List(
          PenUp,
          PlotAbs((x + xb*radius, y - yb*radius)),
          PenDown,
          PlotRel((2*xb*radius, 2*yb*radius))
        ) ++
          cross(x + r, y, 0, 1, rr, d) ++
          cross(x, y + r, -1, 0, rr, d) ++
          cross(x - r, y, 0, -1, rr, d) ++
          cross(x, y - r, 1,  0, rr, d)
      } else {
        Nil
      }
    }

    val plot = cross(xMax / 2, yMax / 2, 0, 1, yMax * 0.45, 3)

    plot.foreach(cmd => println(cmd.text + ";"))

    HpGl.send(plot)
  }
}
