package tutorial.webapp

import scalajs.js
import js.annotation._
import js.Dynamic.global
import org.scalajs.dom
import dom.document

object TutorialApp {

  case class Edge(u: Int, v: Int)

  type Graph = Set[Edge]

  /* Produce all rotations of given list
  // e.g. rotations(List(1,2,3)) => List(List(2, 3, 1), List(3, 1, 2), List(1, 2, 3))

  def rotations(l: List[Int]): List[List[Int]] =
    unfold( (l.length,l) ){ case (i,a) =>
      if (i == 0) {
        None
      } else {
        val rotated = a.tail ++ List(a.head)
        Some( (rotated,(i-1,rotated)) )
      }
    }._1
  */

  // Helper to work on a tuple, to avoid some explicit matches in the body of the code

  def mapValue[A,B,S](t: (A,S))(f: A => B): (B,S) = t match { case (a,s) => (f(a),s) }

  // `unfold` starts from a seed value and accumulates a list as long as
  // the supplied function, given the current seed, produces a new value and transformed seed.

  def unfold[B,S](z: S)(f: S => Option[(B,S)]): (List[B],S) =
    f(z).fold[(List[B],S)]( (Nil,z) ){
      case (b,s) => mapValue(unfold(s)(f))(b :: _)
    }

  def graphs(vertices: Int): IndexedSeq[Graph] = {
    // Construct set of all undirected edges for given number of vertices
    // NOTE: u > v (equivalent to a triangular adjacency matrix).
    val allEdges = for {
      v <- 1 until vertices
      u <- (v+1) to vertices
    } yield Edge(u,v)

    // Get all permutations of vertex labels as a list of maps
    val vs = 1 to vertices
    val labelPerms: List[Map[Int, Int]] = vs.permutations.map(p => vs.zip(p).toMap).toList

    (0 to allEdges.size).flatMap{ k =>
      // Get all subgraphs formed by combinations of k edges from the set of all edges.
      // Some of these graphs will be isomorphic to one another.
      // We can find the redundant isomorphisms by permuting LABELS of each vertex in each subgraph.

      val subgraphs = allEdges.combinations(k).map(_.toSet).toSet

      val grafs = unfold[Graph,Set[Graph]](subgraphs)( gs =>
        // When gs is empty, unfolding will terminate with all subgraphs found so far.
        gs.headOption.map{ g =>
          // g is an arbitrary subgraph (selected from all combinations of k edges as above).
          // Identify its isomorphisms by applying every possible permutation of labels.
          val perms =
            labelPerms.map{ p =>   // p ranges across every permutation (mapping) of labels
              g.map{ case Edge(u,v) => // (u,v) ranges across each edge in subgraph
                val (u_,v_) = (p(u),p(v))         // permutation might give u <= v, i.e. outside the domain of edges,
                if(u_ > v_) Edge(u_,v_) else Edge(v_,u_)  // but still a logical edge (v,u)
              }
            }
          // Removing all label-permuted subgraphs from the unfolding state
          // excludes them from future results for this k.
          (g,gs -- perms)  // produce one subgraph g along with updated state, and continue to unfold
        }
      )._1

      // This first step has chosen an arbitrary permutation of labels for each subgraph.

      // For aesthetic reasons we don't stop there;
      // we want to prefer those which minimise the number of edge crossings in our
      // arbitrarily chosen display format (vertices equally spaced around a circle).

      // TODO: Among those with minimal crossings, we also want to prefer the "shortest" total
      //       edge length given our display format.

      // Try permutations of vertices to minimise the number of crossed edges.
      // Of course this is an expensive, brute force search, but because vertices
      // are arranged in a circle, can skip those which are 'rotations'
      // or reversals of one another.

      // In every one of these permutations in `circPerms`, vertex 1 remains fixed ...
      // given this, we know that none are reversals or rotations of another.

      // FIXME: This shouldn't be ALL permutations, but only those where vertex 1 maps to itself?
      val circPerms: List[Map[Int, Int]] =
        (2 to vertices).toList.permutations.map(1 :: _).map(p => vs.zip(p).toMap).toList

      grafs.map { g =>
        val bestPerm = circPerms.foldLeft( (Map[Int,Int](),Int.MaxValue) ){
          case ((best,min), m) => // each m is a candidate label mapping

            // TODO: Can probably optimise this a bit by stopping
            //       as soon as a zero crossing permutation is found,
            //       since that can't be improved upon by checking the rest.

            // take every pair of edges and see if they cross under the mapping
            val crossings = g.toList.combinations(2).map {
              case List(Edge(u0, v0), Edge(u1, v1)) =>
                // A pair of edges will either be in the form:
                // a -> b -> b -> c   if they share one point (this can NEVER be a crossing)
                // or a -> b, c -> d  if they are not connected

                if (u0 == u1 || u0 == v1 || v0 == u1 || v0 == v1) {
                  0 // edges share a vertex
                } else {
                  // by sorting vertices by label regardless of the edge they belong to,
                  // we only have to determine whether a vertex from one edge lies in between
                  // the sorted labels of the other edge -- then the two edges must cross,
                  // when laid out in order around our chosen circular sequence.

                  // These are the cases, marked by the edge owning vertices laid out in order:
                  //   0 0 1 1 (no crossing)
                  //   1 1 0 0 (no crossing)
                  //   0 1 1 0 (no crossing)
                  //   1 0 0 1 (no crossing)
                  //   0 1 0 1 (crossing) -- hint: first and third are the same,
                  //   1 0 1 0 (crossing) --       and second and fourth are the same

                  // map labels according to the permutation being tested
                  val (u0_, v0_, u1_, v1_) = (m(u0), m(v0), m(u1), m(v1))
                  // pick one of the edges and sort its vertex labels
                  val (u,v) = if (u0_ < v0_) (u0_,v0_) else (v0_,u0_)
                  // take one vertex from the other edge and see if it's between test edge vertices
                  val a = if (u1_ > u && u1_ < v) 1 else 0
                  // do the same for the 2nd vertex from the other edge
                  val b = if (v1_ > u && v1_ < v) 1 else 0
                  // there is a crossing if only one of the vertices is between
                  a ^ b
                }
            }.sum

            if (crossings < min) {
              (m, crossings)
            } else {
              (best, min)
            }
          }._1
        g.map{ case Edge(u,v) => Edge(bestPerm(u),bestPerm(v)) }
      }
    }
  }

  def appendPre(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("pre")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }


  @js.native
  @JSGlobalScope
  object MyApiImpl extends js.Object {
    def render(order: Int, gs: js.Array[js.Array[js.Array[Int]]]): Unit = js.native
  }

  def main(args: Array[String]): Unit = {

    import js.JSConverters._

    dom.window.onload = _ => {
      global.console.log("LOADED")

      val order = 6
      val gs = graphs(order)
      //appendPre(document.body, gs.map(_.toString).mkString("\n"))

      MyApiImpl.render(order, gs.map(_.map { case Edge(u, v) => js.Array(u, v) }.toJSArray).toJSArray)
    }
  }
}
