package tutorial.webapp

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import org.scalajs.dom.html

import util.Random

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

  // The most simplistic way to render; each wall is a separate line
  def mazeLines(w: Int, h: Int, walls: Set[(Int, Int, Dir)]): Iterable[Line] = {
    (0 to h).flatMap(j =>
      (0 to w).flatMap(i =>
        (if (walls.contains((i, j, Dir.n))) List(Line(i, j, i+1, j)) else Nil) ++
        (if (walls.contains((i, j, Dir.w))) List(Line(i, j, i, j+1)) else Nil)))
  }

  /* For vector displays, we should optimise the display list,
     drawing as few, and as long, lines as possible.

     A strategy to do this:
     Consider the lines to be drawn as edges in the graph that can be directly defined from the wall set.
     Until there are no degree 1 nodes left in the graph:
       For each degree 1 node, start a new path at the node, follow the single edge from that node
       and continue following edges in the same direction until reaching a node with no edges
       continuing in that direction.
       Add the line segment formed by the followed edges and eliminate these edges from the graph.
       If there is a single edge from that node, begin following it in that direction.
       Otherwise this path is finished.

     The remaining edges are part of closed paths.

     As long as any edges remain:
       Pick an arbitrary node, then follow one edge from it, in the same direction as far as we can,
       collecting the edges into a single new segment.
       Then start a new direction and do the same, until the starting node is reached.
   */

  def graph(walls: Set[(Int, Int, Dir)]) = {
    // Nodes are tuples (i,j)
    // maintain a neighbours set for all nodes
    walls.foldLeft(
      //Set[(Int,Int,Int,Int)](), // edges
      Map[(Int,Int),Set[(Int,Int)]]() // node -> neighbours
    ){
      case (nbrs,(i,j,d)) =>
        val newNbrs = nbrs.getOrElse((i,j), Set())
        d match {
          // consider the cell coordinate to be the west end of "north" wall, edge running to east;
          // and north end of "west" wall, edge running to south.
          case Dir.North =>
            val eastNbrs = nbrs.getOrElse((i+1,j), Set())
            //edges + ((i,j,i+1,j)),
            nbrs + ((i,j) -> (newNbrs + ((i+1,j)))) + ((i+1,j) -> (eastNbrs + ((i,j))))
          case Dir.West =>
            val southNbrs = nbrs.getOrElse((i,j+1), Set())
            //edges + ((i,j,i,j+1)),
            nbrs + ((i,j) -> (newNbrs + ((i,j+1)))) + ((i,j+1) -> (southNbrs + ((i,j))))
        }
    }
  }

  def linkEdges(nbrs: Map[(Int,Int),Set[(Int,Int)]]) = {
    def sgn(v: Int) = if (v < 0) -1 else if (v > 0) 1 else 0

    // Result is a single path, plus a set of new nodes to follow
    def follow(
                head: (Int,Int),
                prev: Option[(Int,Int)],
                dx: Int, dy: Int
              ): (List[(Int,Int)],Option[(Int,Int)]) = {
      val ns = nbrs(head)
      val forward = ns -- prev.toSet // ignore the node we're coming from
      val nextSameDir = (head._1+dx,head._2+dy)
      if (forward.contains(nextSameDir)) {
        // can continue in same direction
        // FIXME: When we do this, we need to eliminate this neighbour (head) from the neighbour sets
        //        for each path that was NOT chosen.
        //        This will create disconnected (cut off) paths that need to be followed later!
        follow(nextSameDir, Some(head), dx, dy)
      } else if (forward.size == 1) { // change direction
        val nextNewDir = forward.head
        val (path,endpoint) = follow(nextNewDir, Some(head), sgn(nextNewDir._1 - head._1), sgn(nextNewDir._2 - head._2))
        (head :: path, endpoint)
      } else { // cannot continue - reached end of path or a T-junction
        (List(head), Some(head))
      }
    }

    // take all degree one nodes. Collect the path endpoints so we do not reprocess them.
    val (seen,paths) = nbrs.toList.filter{ case (_,ns) => ns.size == 1 }
      .foldLeft( (Set[(Int,Int)](),List[List[(Int,Int)]]()) ){ case ((seen,paths),(node,ns)) =>
        if (seen.contains(node)) { // skip this node; we have already reached it
          (seen,paths)
        } else {
          // ns has exactly one member
          val next = ns.head // get that member
          val (path, endpoint) = follow(node, None, next._1 - node._1, next._2 - node._2)
          (seen ++ endpoint.toSet, (node :: path) :: paths)
        }
      }

    // Now take all degree 3 nodes (T junctions). pick the direction that isn't collinear
    val (_,paths2) = nbrs.toList.filter{ case (_,ns) => ns.size == 3 }
      .foldLeft( (seen,List[List[(Int,Int)]]()) ){ case ((seen,paths),(node,ns)) =>
        if (seen.contains(node)) { // skip this node; we have already reached it
          (seen,paths)
        } else {
          val (a,b) = node
          // if node is at (a,b), neighbours will be (a,b-1),(a,b+1),(a±1,b) for a departure left or right
          // if node is at (a,b), neighbours will be (a-1,b),(a+1,b),(a,b±1) for a departure up or down
          val eastWest = ns.filter{ case (i,j) => i == a }
          val northSouth = ns.filter{ case (i,j) => j == b }
          val ignoreDirections = if (eastWest.size > 1) eastWest else northSouth
          val next = (ns -- ignoreDirections).head
          val (path, endpoint) = follow(node, None, next._1 - a, next._2 - b)
          (seen ++ endpoint.toSet, (node :: path) :: paths)
        }
      }

    (paths,paths2)
  }


  @JSExportTopLevel("runMaze")
  def runMaze(c: html.Canvas): Unit = {
    import js.JSConverters._

    type Ctx2D = dom.CanvasRenderingContext2D
    val ctx = c.getContext("2d").asInstanceOf[Ctx2D]

    ctx.strokeStyle = "#555"
    ctx.lineWidth = 20
    ctx.lineCap = "round"
    ctx.globalCompositeOperation = "lighter"

    val (w, h) = (24, 24)
    val k = c.height/(h+2.0)
    val xpos = (c.width - k*w)/2.0

    val walls = maze(w, h)._1

    // Draw all walls

    ctx.beginPath()
    mazeLines(w, h, walls).foreach { line =>
      ctx.moveTo(xpos + k*line.x0, k*(1+line.y0))
      ctx.lineTo(xpos + k*line.x1, k*(1+line.y1))
    }
    ctx.stroke()

    //ctx.strokeStyle = "#7dff9a"
    ctx.strokeStyle = "#444"
    ctx.lineWidth = 4

    val (p1,p2) = linkEdges(graph(walls))

    // Draw the paths linked from degree-1 points

    p1.foreach{ path =>
      val (x,y) = (xpos + k*path.head._1, k*(1+path.head._2))
      ctx.beginPath()
      ctx.moveTo(x-5,y-5)
      ctx.lineTo(x+5,y+5)
      ctx.moveTo(x-5,y+5)
      ctx.lineTo(x+5,y-5)
      ctx.moveTo(x,y)
      path.tail.foreach{ case (i,j) => ctx.lineTo(xpos + k*i, k*(1+j)) }
      ctx.stroke()
    }

    ctx.strokeStyle = "#600"

    // Draw the paths linked from degree-3 (T junctions)

    p2.foreach{ path =>
      val (x,y) = (xpos + k*path.head._1, k*(1+path.head._2))
      ctx.beginPath()
      ctx.moveTo(x-5,y-5)
      ctx.lineTo(x+5,y+5)
      ctx.moveTo(x-5,y+5)
      ctx.lineTo(x+5,y-5)
      ctx.moveTo(x,y)
      path.tail.foreach{ case (i,j) => ctx.lineTo(xpos + k*i, k*(1+j)) }
      ctx.stroke()
    }

  }
}

object VectorMaze {
  import MazeDemo._
  import HpGl._

  def plot(): List[HpGlCommand] = {
    val (w, h) = (24, 24)
    val walls = maze(w, h)._1

    val k = yMax/(h+2.0)
    val xpos = (xMax - k*w)/2.0
    val ypos = (yMax - k*h)/2.0

    val (p1,p2) = linkEdges(graph(walls))

    def abs(n: (Int,Int)): HpGlCommand = PlotAbs( (xpos + k*n._1, ypos + k*n._2) )

    (p1 ++ p2 ++ List(List((0,0),(w,0),(w,h),(0,h),(0,0))))
      .flatMap(nodes => List(PenUp, abs(nodes.head), PenDown) ++ nodes.tail.map(abs))
  }

  def run(): Unit = {
    HpGl.send(Page :: plot())
  }

}

