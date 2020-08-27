package tutorial.webapp

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.html

import scala.annotation.tailrec
import scala.util.Random

object MazeDemo {

  sealed trait Dir

  object Dir {
    case object North extends Dir
    case object West extends Dir
    case object South extends Dir
    case object East extends Dir

    def n: Dir = North
    def w: Dir = West
    def s: Dir = South
    def e: Dir = East
  }

  def maze(w: Int, h: Int): (Set[(Int, Int, Dir)], Set[(Int, Int)]) = {

    val walls =
      (for {i <- 0 until w; j <- 0 until h}
        yield List((i, j, Dir.n), (i, j, Dir.w))
      ).flatten.toSet ++
      (0 until w).map( (_, h, Dir.n) ) ++ // add bottom row of edges
      (0 until h).map( (w, _, Dir.w) ) // add right column of edges

    val allCells = (for {i <- 0 until w; j <- 0 until h} yield (i,j)).toSet
    // To block out areas from being visited (like 'walls'), filter out some cells:
    // .filter(_ => Random.nextDouble() > 0.25).toSet

    val wallPerms = List(Dir.w, Dir.e, Dir.n, Dir.s).permutations.toVector
    def randomWallPerm = wallPerms( Math.abs(Random.nextInt()) % wallPerms.size )

    def step(i: Int, j: Int, walls: Set[(Int, Int, Dir)], unvisited: Set[(Int, Int)]): (Set[(Int, Int, Dir)],Set[(Int, Int)]) = {
      if (unvisited.isEmpty) {
        (walls,unvisited)
      } else {
        randomWallPerm.foldLeft(
          (walls, unvisited - ((i, j)))
        ) { case ((w, u), d) => // remove wall and change position
          val (newi, newj, newWalls) = d match {
            case Dir.North => (i, j - 1, w - ((i, j, Dir.n)))
            case Dir.West => (i - 1, j, w - ((i, j, Dir.w)))
            case Dir.South => (i, j + 1, w - ((i, j + 1, Dir.n)))
            case Dir.East => (i + 1, j, w - ((i + 1, j, Dir.w)))
          }
          if (u.contains( (newi,newj) ))
            step(newi, newj, newWalls, u)
          else
            (w,u)
        }
      }
    }

    (step(0, 0, walls, allCells)._1, allCells)
  }

  def printMaze(w: Int, h: Int, maze: (Set[(Int, Int, Dir)], Set[(Int, Int)])): Unit = {
    val (walls,unvisited) = maze
    (0 to h).foreach { j =>
      def fill(i: Int) = if (i >= w || j >= h || unvisited.contains((i,j))) "   " else "XXX"
      println((0 to w).map(i => "o" + (if (walls.contains((i, j, Dir.n))) "---" else "   ")).mkString)
      println((0 to w).map(i => (if (walls.contains((i, j, Dir.w))) "|"+fill(i) else " "+fill(i))).mkString)
    }
  }

  def mazeLines(w: Int, h: Int, walls: Set[(Int, Int, Dir)]): Iterable[Line] = {
    (0 to h).flatMap(j =>
      (0 to w).flatMap(i =>
        (if (walls.contains((i, j, Dir.n))) List(Line(i, j, i+1, j)) else Nil) ++
        (if (walls.contains((i, j, Dir.w))) List(Line(i, j, i, j+1)) else Nil)))
  }

  @JSExportTopLevel("runMaze")
  def runMaze(c: html.Canvas): Unit = {
    import js.JSConverters._

    type Ctx2D = dom.CanvasRenderingContext2D
    val ctx = c.getContext("2d").asInstanceOf[Ctx2D]

    ctx.strokeStyle = "#7dff9a"
    ctx.lineWidth = 3
    ctx.lineCap = "round"
    ctx.globalCompositeOperation = "lighter"

    ctx.beginPath()
    val (w, h) = (50, 50)
    val k = c.height/(h+2.0)
    val xpos = (c.width - k*w)/2.0
    mazeLines(w, h, maze(w, h)._1).foreach { line =>
      ctx.moveTo(xpos + k*line.x0, k*(1+line.y0))
      ctx.lineTo(xpos + k*line.x1, k*(1+line.y1))
    }
    ctx.stroke()

  }
}
