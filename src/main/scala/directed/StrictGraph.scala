package directed

import scala.collection.Seq
import scala.collection.Set
import scala.collection.Map

import scala.annotation.tailrec

/** Trait for a directed ''and strict'' graph, i.e. without loop nor parallel arcs */
trait StrictGraph[V] {
  /* QUERY METHODS */

  /** The set of all vertices of the graph */
  val vertices: Set[V]

  /** The set of all     arcs of the graph */
  val arcs: Set[Arc[V]]

  /** The set of all vertices with arcs incoming from input vertex
    *
    * @param v vertex
    * @return [[None]] if `v` is not an actual vertex, the set of all successors of `v` otherwise
    */
  def successorsOf(v: V): Option[Set[V]]

  /** The number of incoming arcs to input vertex
    *
    * @param v vertex
    * @return [[None]] if `v` is not an actual vertex, the inner degree of `v` otherwise
    */
  def inDegreeOf(v: V): Option[Int] = {
    if (vertices contains v)
      Some(arcs count { _._2 == v })
    else
      None
  }

  /** The number of outcoming arcs to input vertex
    *
    * @param v vertex
    * @return [[None]] if `v` is not an actual vertex, the outer degree of `v` otherwise
    */
  def outDegreeOf(v: V): Option[Int] = successorsOf(v) map { _.size }

  /** The number of adjacent vertices to input vertex
    *
    * @param v vertex
    * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
    */
  def degreeOf(v: V): Option[Int] = {
    if (vertices contains v)
      None
    else
      Some(outDegreeOf(v).get - inDegreeOf(v).get)
  }

  /* VERTEX OPERATIONS */

  /** Add vertex to graph
    *
    * @param v new vertex
    * @return the graph with new vertex `v`
    *         if `v` is an actual vertex of graph, return input graph
    */
  def +(v: V): StrictGraph[V]

  /** Remove vertex from graph
    *
    * @param v new vertex
    * @return the graph without vertex `v`
    *         if `v` is not an actual vertex of graph, return input graph
    */
  def -(v: V): StrictGraph[V]

  /* ARC OPERATIONS */

  /** Add arc to graph (also add arc ends as new vertices if necessary)
    *
    * @param a new arc
    * @return the graph with new arc `e`
    *         if `e` is an actual arc of graph, return input graph
    */
  def +|(a: Arc[V]): StrictGraph[V]

  /** Remove arc from graph (does NOT remove ends)
    *
    * @param a new arc
    * @return the graph without arc `e`
    *         if `e` is not an actual arc of graph, return input graph
    */
  def -|(a: Arc[V]): StrictGraph[V]

  /** Remove all arcs from graph but keep same vertices
    *
    * @return graph with same vertices without any arc
    */
  def withoutArc: StrictGraph[V]

  /** Add all possible arc with same vertices
    *
    * @return graph with same vertices and all possible arcs
    */
  def withAllArcs: StrictGraph[V]

  /* SEARCH METHODS */

  /** A topological order of the vertex set (if exists) */
  lazy val topologicalOrder: Option[Seq[V]] = {
    @tailrec
    def createTopologicalOrder(graph: StrictGraph[V], order: Seq[V]): Option[Seq[V]] = {
      val leaves = graph.vertices.filter(graph.successorsOf(_).get.isEmpty)
      if (leaves.isEmpty)
        if (graph.vertices.isEmpty)
          Some(order)
        else
          None
      else {
        @tailrec
        def treatLeaves(graph: StrictGraph[V], order: Seq[V], leaves: Set[V]): (StrictGraph[V], Seq[V]) = {
          if (leaves.isEmpty)
            (graph, order)
          else {
            val leaf = leaves.head
            treatLeaves(graph - leaf, leaf +: order, leaves.drop(1))
          }
        }

        val res = treatLeaves(graph, order, leaves)
        createTopologicalOrder(res._1, res._2)
      }
    }

    createTopologicalOrder(this, Seq.empty)
  }

  /** Computes a shortest path between two vertices
    *
    * @param valuation valuation of graph
    * @param start     origin      of path
    * @param end       destination of path
    * @return [[None]] if there is no path from `start` to `end`, the shortest path and its valuation otherwise
    */
  def shortestPath(valuation: Map[Arc[V], Double])(start: V, end: V): Option[(Seq[V], Double)] = {
    val indexedVertices = vertices.toIndexedSeq

    @tailrec
    def dijkstra(predecessors: Array[Option[Int]], degrees: Array[Double], seen: Array[Boolean]): Option[(Array[Option[Int]], Array[Double])] = {
      @tailrec
      def calculateArgmin(i: Int, argmin: Int): Int = {
        if (i == indexedVertices.size)
          argmin
        else
          calculateArgmin(i + 1, {
            if (!seen(i))
              if ((argmin == -1) || (degrees(i) < degrees(argmin))) i else argmin
            else
              argmin
          })
      }

      val argmin = calculateArgmin(0, -1)
      if (argmin == -1)
        None
      else {
        val v = indexedVertices(argmin)
        if (v == end)
          Some((predecessors, degrees))
        else {
          @tailrec
          def treatSuccessors(predecessors: Array[Option[Int]], degrees: Array[Double], successors: Set[V]): (Array[Option[Int]], Array[Double]) = {
            if (successors.isEmpty)
              (predecessors, degrees)
            else {
              val successor = successors.head
              val indexOfSuccessor = indexedVertices indexOf successor

              if (!seen(indexOfSuccessor)) {
                val newDegree = degrees(argmin) + valuation(Arc[V](v, successor))
                if (newDegree < degrees(indexOfSuccessor))
                  treatSuccessors(predecessors.updated(indexOfSuccessor, Some(argmin)), degrees.updated(indexOfSuccessor, newDegree), successors.drop(1))
                else
                  treatSuccessors(predecessors, degrees, successors.drop(1))
              }
              else
                treatSuccessors(predecessors, degrees, successors.drop(1))
            }
          }

          val res = treatSuccessors(predecessors, degrees, successorsOf(v).get)
          dijkstra(res._1, res._2, seen.updated(argmin, true))
        }
      }
    }

    if (!vertices.contains(start) || !vertices.contains(end))
      None
    else {
      val res = dijkstra(Array.fill(indexedVertices.size) { None }, Array.fill(indexedVertices.size) { Double.PositiveInfinity }.updated(indexedVertices.indexOf(start), 0), Array.fill(indexedVertices.size) { false })
      if (res.isEmpty)
        None
      else {
        @tailrec
        def path(i: Int, value: Seq[V]): Seq[V] = {
          if (i == indexedVertices.indexOf(start))
            start +: value
          else
            path(res.get._1(i).get, indexedVertices(i) +: value)
        }

        val indexOfEnd = indexedVertices.indexOf(end)
        Some((path(indexOfEnd, Seq.empty[V]), res.get._2(indexOfEnd)))
      }
    }
  }
  /* toString-LIKE METHODS */

  /** @inheritdoc */
  override lazy val toString : String = s"({${vertices mkString ", "}}, {${arcs mkString ", "}})"

  /** Graph representation in DOT language */
  lazy val toDOTString : String = {
    "strict graph {\n" +
      "    // Edges\n" +
      (arcs foldLeft "    ") { _ + _.toDOTString + "\n    " } + "\n" +
      "    // Vertices\n" +
      vertices.mkString("    ", "\n    ", "\n") +
      "  }\n"
  }
}