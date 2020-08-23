package tutorial.webapp

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSGlobalScope}
import org.scalajs.dom
import org.scalajs.dom.html

object FractalDemo {

  def plot(xMax: Double, yMax: Double, depth: Int): List[Line] = {

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
      cross(xMax/2, yMax/2, radius, 0, depth) /*++
      cross(xMax/2, yMax/2, 0, radius, 4) ++
      cross(xMax/2, yMax/2, -radius, 0, 4) ++
      cross(xMax/2, yMax/2, 0, -radius, 4)*/
  }

  @js.native
  @JSGlobalScope
  object Js extends js.Object {
    //def renderFrac(lines: js.Array[js.Array[Double]]): Unit = js.native
  }

  @JSExportTopLevel("runFrac")
  def runFrac(c: html.Canvas): Unit = {
    import js.JSConverters._

    type Ctx2D = dom.CanvasRenderingContext2D
    val ctx = c.getContext("2d").asInstanceOf[Ctx2D]

    ctx.strokeStyle = "#7dff9a"
    ctx.lineWidth = 3
    ctx.lineCap = "round"
    ctx.globalCompositeOperation = "lighter"

    plot(c.width, c.height, 5).foreach{ case Line(x0, y0, x1, y1) =>
      ctx.beginPath()
      ctx.moveTo(x0, y0)
      ctx.lineTo(x1, y1)
      ctx.stroke()
    }

    //Js.renderFrac(plot(c.width, c.height).map { case Line(x0, y0, x1, y1) => js.Array(x0, y0, x1, y1) }.toJSArray)
  }
}
