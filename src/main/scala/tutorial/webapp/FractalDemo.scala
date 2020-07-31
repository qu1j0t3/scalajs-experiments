package tutorial.webapp

object FractalDemo {
  import HpGl._

  def run(): Unit = {

    def cross(x: Double, y: Double, xb: Double, yb: Double, depth: Int): List[HpGlCommand] = {
      if (depth > 0) {
        val d = depth - 1
        val x90 = -yb; val y90 = xb;
        val x_90 = yb; val y_90 = -xb;

        List(
          PenUp,
          PlotAbs((x+x_90, y+y_90)),
          PenDown,
          PlotAbs((x+x90, y+y90))
        ) ++
          cross(x + xb*0.6, y + yb*0.6, xb*0.4, yb*0.4, d) ++
          cross(x + x90*0.6, y + y90*0.6, x90*0.4, y90*0.4, d) ++
          cross(x + x_90*0.6, y + y_90*0.6, x_90*0.4, y_90*0.4, d)
      } else {
        Nil
      }
    }

    val radius = yMax * 0.45
    val plot = List(PenUp, PlotAbs((xMax/2 - radius, yMax/2)), PenDown, PlotRel((radius*2, 0))) ++
      cross(xMax/2, yMax/2, radius, 0, 5) /*++
      cross(xMax/2, yMax/2, 0, radius, 4) ++
      cross(xMax/2, yMax/2, -radius, 0, 4) ++
      cross(xMax/2, yMax/2, 0, -radius, 4)*/

    plot.foreach(cmd => println(cmd.text + ";"))

    HpGl.send(plot)
  }
}
