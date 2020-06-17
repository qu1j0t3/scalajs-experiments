package tutorial.webapp

import scalajs.js
import js.annotation._
import js.Dynamic.global
import org.scalajs.dom
import dom.document

object TutorialApp {

  type Edge = (Int,Int)
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
    // (equivalent to a triangular adjacency matrix).
    val allEdges = for {
      v <- 1 until vertices
      u <- (v+1) to vertices
    } yield (u,v)
    // NOTE: u > v

    val permMap: List[Int] => Map[Int, Int] =
      _.zipWithIndex.map{ case (label,idx) => (idx+1) -> label }.toMap

    // Get all permutations of vertex labels as a list of maps
    val labelPerms: List[Map[Int, Int]] = (1 to vertices).toList.permutations.map(permMap).toList

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
              g.map{ case (u,v) => // (u,v) ranges across each edge in subgraph
                val (u_,v_) = (p(u),p(v))         // permutation might give u <= v, i.e. outside the domain of edges,
                if(u_ > v_) (u_,v_) else (v_,u_)  // but still a logical edge (v,u)
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

      val circPerms: List[Map[Int, Int]] =
        (2 to vertices).toList.permutations.map(1 :: _).map(permMap).toList

      grafs.map { g =>
        val bestPerm = circPerms.foldLeft( (Map[Int,Int](),Int.MaxValue) ){
          case ((best,min), m) => // each m is a candidate label mapping
            // take every pair of edges and see if they cross under the mapping
            val crossings = g.toList.combinations(2).map {
              case List((u0, v0), (u1, v1)) =>
                val (u0_,v0_,u1_,v1_) = (m(u0),m(v0),m(u1),m(v1))  // map labels for the 2 edges
                // I have to be frank, I can't recall how this works right now. TODO: Update this comment
                // The moral is, write comments at the time you have it fresh in mind!
                val m1 = Map(u0_ -> 0, v0_ -> 0, u1_ -> 1, v1_ -> 1)
                List(u0_, v0_, u1_, v1_).distinct.sorted match {
                  case List(a, b, c, d) if m1(a) == m1(c) && m1(b) == m1(d) => 1
                  case _ => 0
                }
            }.sum
            if (crossings < min) {
              (m, crossings)
            } else {
              (best, min)
            }
          }._1
        g.map{ case (u,v) => (bestPerm(u),bestPerm(v)) }
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

      MyApiImpl.render(order, gs.map(_.map { case (u, v) => js.Array(u, v) }.toJSArray).toJSArray)
    }
  }
}
