package tutorial.webapp

import scalajs.js

import js.JSApp
import js.annotation._
import js.Dynamic.global

import org.scalajs.dom
import dom.document


import org.querki.jquery._

object TutorialApp extends JSApp {

  def mapValue[A,B,S](t: (A,S))(f: A => B): (B,S) = t match {
    case (a,s) => (f(a),s)
  }

  def unfold[B,S](z: S)(f: S => Option[(B,S)]): (List[B],S) =
    f(z).fold[(List[B],S)]( (Nil,z) ){
      case (b,s) => mapValue(unfold(s)(f))(b :: _)
    }

  type Edge = (Int,Int)
  type Graph = Set[Edge]

  def graphs(vertices: Int): IndexedSeq[Graph] = {
    // Construct set of all undirected edges for given number of vertices
    // (equivalent to a triangular adjacency matrix).
    val allEdges = for {
      v <- 1 until vertices
      u <- (v+1) to vertices
    } yield (u,v)

    // Get all permutations of vertex labels as a list of maps
    val labelPerms = (1 to vertices).permutations
      .map(_.zipWithIndex.map{ case (label,idx) => (idx+1) -> label }.toMap).toList

    (0 to allEdges.size).flatMap{ k =>
      // Get all subgraphs formed by combinations of k edges from the set of all edges.
      // Some of these graphs will be isomorphic to one another, so must be filtered further.
      val subgraphs = allEdges.combinations(k).map(_.toSet).toSet

      val grafs = unfold[Graph,Set[Graph]](subgraphs)( gs =>
        // When gs is empty, unfolding will terminate with all subgraphs found so far.
        gs.headOption.map{ g =>
          // g is an arbitrary subgraph (selected from all combinations of k edges as above).
          // Identify its isomorphisms by applying every possible permutation of labels.
          val perms =
            labelPerms.map{ p =>           // p ranges across every permutation of labels
              g.map{ case (u,v) =>         // (u,v) ranges across each edge in subgraph
                val (u_,v_) = (p(u),p(v))  // permutation might give u <= v, i.e. outside the domain of edges,
                if(u_ > v_) (u_,v_) else (v_,u_)  // but still a logical edge (v,u)
              }
            }
          // Removing the label-permuted subgraphs from the unfolding state
          // excludes them from future results for this k.
          (g,gs -- perms)  // produce one subgraph g along with updated state, and continue to unfold
        }
      )._1

      // Try every permutation of vertices to minimise the number of crossed edges.
      // Of course this is an expensive, brute force search, but because vertices
      // are arranged in a circle, we could omit trying
      // all perms which are 'rotations' of forward or reverse order.
      grafs.map { g =>
        val bestPerm = labelPerms.map{ m =>
          val crossings = g.toList.combinations(2).map {
            case List((u0, v0), (u1, v1)) =>
              val (u0_,v0_,u1_,v1_) = (m(u0),m(v0),m(u1),m(v1))
              val m1 = Map(u0_ -> 0, v0_ -> 0, u1_ -> 1, v1_ -> 1)
              List(u0_, v0_, u1_, v1_).distinct.sorted match {
                case List(a, b, c, d) if m1(a) == m1(c) && m1(b) == m1(d) => 1
                case _ => 0
              }
          }
          (m,crossings.sum)
        }.minBy(_._2)._1
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

  def main(): Unit = {

    import js.JSConverters._

    //appendPre(document.body, graphs(6).map(_.toString).mkString("\n"))

    $ { () =>
      global.console.log("LOADED")

      val order = 5
      val gs = graphs(order)

      MyApiImpl.render(order, gs.map(_.map { case (u, v) => js.Array(u, v) }.toJSArray).toJSArray)
    }
  }
}
