package tutorial.webapp

import org.scalajs.dom.document
import tutorial.webapp.GraphEnumerate.{Edge, appendPre, graphs}

import scala.scalajs.js
import scala.scalajs.js.Dynamic.global
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobalScope}

/**
 * A disconnected line
 */
case class Line(x0: Double, y0: Double, x1: Double, y1: Double) {
  def asHpGl: List[HpGlCommand] = List(PenUp, PlotAbs((x0, y0)), PenDown, PlotAbs((x1, y1)))
}

object FractalDemo {
  import HpGl._

  def plot = {

    def cross(x: Double, y: Double, xb: Double, yb: Double, depth: Int): List[Line] = {
      if (depth > 0) {
        val d = depth - 1
        val x90 = -yb; val y90 = xb;
        val x_90 = yb; val y_90 = -xb;

        List(Line(x+x_90, y+y_90, x+x90, y+y90)) ++
          cross(x + xb*0.6, y + yb*0.6, xb*0.4, yb*0.4, d) ++
          cross(x + x90*0.6, y + y90*0.6, x90*0.4, y90*0.4, d) ++
          cross(x + x_90*0.6, y + y_90*0.6, x_90*0.4, y_90*0.4, d)
      } else {
        Nil
      }
    }

    val radius = yMax * 0.45

    List(Line(xMax/2 - radius, yMax/2, xMax/2 + radius, yMax/2)) ++
      cross(xMax/2, yMax/2, radius, 0, 5) /*++
      cross(xMax/2, yMax/2, 0, radius, 4) ++
      cross(xMax/2, yMax/2, -radius, 0, 4) ++
      cross(xMax/2, yMax/2, 0, -radius, 4)*/
  }

  @js.native
  @JSGlobalScope
  object Js extends js.Object {
    def renderFrac(lines: js.Array[js.Array[Double]]): Unit = js.native
  }

  @JSExportTopLevel("runFrac")
  def runFrac(): Unit = {
    import js.JSConverters._

    val t0 = global.Date.now()
    val order = 5
    val gs = graphs(order)
    val t1 = global.Date.now()
    appendPre(document.body, (t1 - t0).toString)

    Js.renderFrac(plot.map { case Line(x0, y0, x1, y1) => js.Array(x0, y0, x1, y1) }.toJSArray)
  }
}
