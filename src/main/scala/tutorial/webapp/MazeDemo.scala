package tutorial.webapp

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.html

import scala.annotation.tailrec
import scala.util.Random

object MazeDemo {
  /**
   * From Jamis Buck, the description of maze generation by recursive backtracking:
   *
   * 1. Choose a starting point
   * 2. Crawl forward: randomly choose a wall and delete it if adjacent cell has not been visited
   * 3. if all adjacent cells have been visited, backtrack
   * 4. algorithm ends when we have backtracked to starting point
   *
   * Data:
   * * Set of walls
   * * Visited flag per cell
   *
   * Cells are indexed 0,0 top left, i,j where i increases to the right (east), j increases down (south)
   * Walls are represented as (0,0,false) is West wall from 0,0; (0,0,true) is North wall from 0,0
   */

  sealed abstract class Dir(istep: Int, jstep: Int)

  object Dir {
    case object North extends Dir(0, -1)
    case object West extends Dir(-1, 0)
    case object South extends Dir(0, 1)
    case object East extends Dir(1, 0)

    def n: Dir = North
    def w: Dir = West
    def s: Dir = South
    def e: Dir = East
  }

  def maze(w: Int, h: Int): Set[(Int, Int, Dir)] = {

    val walls =
      (for {i <- 0 until w; j <- 0 until h}
        yield List((i, j, Dir.n), (i, j, Dir.w))
      ).flatten.toSet ++
      (0 until w).map( (_, h, Dir.n) ) ++ // add bottom row of edges
      (0 until h).map( (w, _, Dir.w) ) // add right column of edges

    val allCells = (for {i <- 0 until w; j <- 0 until h} yield (i,j)).toSet

    def step(i: Int, j: Int, walls: Set[(Int, Int, Dir)], unvisited: Set[(Int, Int)]): (Set[(Int, Int, Dir)],Set[(Int, Int)]) = {
      if (unvisited.isEmpty) {
        (walls,unvisited)
      } else {
        val updatedUnvisited = unvisited - ((i, j))

        val possibleWalls =
          (if (i > 0) List(Dir.w) else Nil) ++
          (if (i < w - 1) List(Dir.e) else Nil) ++
          (if (j > 0) List(Dir.n) else Nil) ++
          (if (j < h - 1) List(Dir.s) else Nil)

        // shuffle available directions
        possibleWalls.map((Random.nextInt(), _)).sortBy(_._1).map(_._2).foldLeft(
          (walls, updatedUnvisited)
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

    step(0, 0, walls, allCells)._1
  }

  def printMaze(w: Int, h: Int, walls: Set[(Int, Int, Dir)]): Unit = {
    (0 to h).foreach { j =>
      println((0 to w).map(i => "+" + (if (walls.contains((i, j, Dir.n))) "---" else "   ")).mkString)
      println((0 to w).map(i => (if (walls.contains((i, j, Dir.w))) "|   " else "    ")).mkString)
    }
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
    ctx.moveTo(0, 0)
    ctx.lineTo(c.width, c.height)
    ctx.moveTo(c.width, 0)
    ctx.lineTo(0, c.height)
    ctx.stroke()

  }
}
