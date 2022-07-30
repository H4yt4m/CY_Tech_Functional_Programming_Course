package undirected

import scala.collection.immutable.Seq
import scala.collection.immutable.Set
import scala.collection.immutable.Map

import scala.annotation.tailrec

/** Trait for an undirected and ''simple'' graph, that is without loop nor parallel edges
  * @tparam V type for vertices
  */
trait SimpleGraph[V] {
    /* QUERY METHODS */

    /** The set of all vertices of the graph */
    val vertices : Set[V]

    /** The set of all    edges of the graph */
    val edges : Set[Edge[V]]

    /** The set of all vertices adjacent to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the set of all neighbors of `v` otherwise (may be empty)
      */
    def neighborsOf(v : V) : Option[Set[V]]

    /** The number of adjacent vertices to input vertex
      * @param v vertex
      * @return [[None]] if `v` is not an actual vertex, the degree of `v` otherwise
      */
    def degreeOf(v : V) : Option[Int] = neighborsOf(v) map { _.size }

    /** Checks if there exists a path between two vertices
      * @param v1 one end of path to search
      * @param v2 other end of path to search
      * @return `true` if `v1` and `v2` are equal or if a path exists between `v1` and `v2`, `false` otherwise
      */
    def hasPath(v1 : V, v2 : V) : Boolean = {
        val indexedVertices = vertices.toIndexedSeq

        def explore(v: V, seen: Array[Boolean], value: Boolean = false): Boolean = {
            @tailrec
            def treatNeighbours(value: Boolean, neighbours: Set[V]): Boolean = {
                if (value)
                    true
                else if (neighbours.isEmpty)
                    false
                else {
                    treatNeighbours({
                        val neighbour = neighbours.head
                        if (!seen(indexedVertices indexOf neighbour))
                            explore(neighbour, seen.updated(indexedVertices indexOf v, true))
                        else
                            false
                    }, neighbours.drop(1))
                }
            }

            if (value || v == v2)
                true
            else
                treatNeighbours(value = false, neighborsOf(v).get)
        }

        if (vertices.contains(v1) && vertices.contains(v2))
            explore(v1, Array.fill(indexedVertices.size) { false })
        else
            false
    }

    /** Checks if graph is connected */
    lazy val isConnected : Boolean = {
        val indexedVertices = vertices.toIndexedSeq

        def explore(v: V, seen: Array[Boolean]): Array[Boolean] = {
            @tailrec
            def treatNeighbours(seen: Array[Boolean], neighbours: Set[V]): Array[Boolean] = {
                if (neighbours.isEmpty) {
                    seen.updated(indexedVertices indexOf v, true)
                } else {
                    treatNeighbours({
                        val neighbour = neighbours.head
                        if (!seen(indexedVertices indexOf neighbour)) {
                            explore(neighbour, seen.updated(indexedVertices indexOf v, true))
                        } else
                            seen
                    }, neighbours.drop(1))
                }
            }
            treatNeighbours(seen, neighborsOf(v).get)
        }

        if (vertices.isEmpty)
            true
        else {
            !explore(indexedVertices(0), Array.fill(vertices.size) { false }).contains(false)
        }
    }

    lazy val isConnected2 : Boolean = !vertices.exists(v1 => vertices exists { v2 => !hasPath(v1, v2) })

    /** Checks if graph is acyclic */
    lazy val isAcyclic : Boolean = !vertices.exists(v1 => neighborsOf(v1).get.exists { v2 => (this -| Edge(v1, v2)).hasPath(v1, v2) })

    /** Checks if graph is a tree */
    lazy val isTree : Boolean = isConnected && isAcyclic

    /* VERTEX OPERATIONS */

    /** Add vertex to graph
      * @param v new vertex
      * @return the graph with new vertex `v`
      *         if `v` is an actual vertex of graph, return input graph
      */
    def +(v : V) : SimpleGraph[V]

    /** Remove vertex from graph
      * @param v new vertex
      * @return the graph without vertex `v`
      *         if `v` is not an actual vertex of graph, return input graph
      */
    def -(v : V) : SimpleGraph[V]

    /* EDGE OPERATIONS */

    /** Add edge to graph (also add edge ends as new vertices if necessary)
      * @param e new edge
      * @return the graph with new edge `e`
      *         if `e` is an actual edge of graph, return input graph
      */
    def +|(e : Edge[V]) : SimpleGraph[V]

    /** Remove edge from graph (does NOT remove ends)
      * @param e new edge
      * @return the graph without edge `e`
      *         if `e` is not an actual edge of graph, return input graph
      */
    def -|(e : Edge[V]) : SimpleGraph[V]

    /** Remove all edges from graph but keep same vertices
      * @return graph with same vertices without any edge
      */
    def withoutEdge : SimpleGraph[V]

    /** Add all possible edge with same vertices
      * @return graph with same vertices and all possible edges
      */
    def withAllEdges : SimpleGraph[V]

    /* VALUATED GRAPH METHODS */

    /** Total value of the graph
      * @param valuation valuation used
      * @return total value of the graph, i.e. sum of values of all edges
      */
    def value(valuation : Map[Edge[V], Double]) : Double = (edges map { valuation(_) }).sum

    /** Minimum spanning tree
      * @param valuation valuation used
      * @return a spanning tree whose value is minimal
      */
    def minimumSpanningTree(valuation : Map[Edge[V], Double]) : SimpleGraph[V] = {
        require(this.isConnected, "Le graphe n'étant pas connexe, l'arbre couvrant minimal n'existe pas.")

        @tailrec
        def kruskal(graph: SimpleGraph[V], seen: Set[Edge[V]] = Set.empty[Edge[V]], n: Int = 1): SimpleGraph[V] = {
            if (n == vertices.size)
                graph
            else {
                val edge = valuation.filterNot(e => seen.contains(e._1)).minBy(_._2)._1
                if (!graph.hasPath(edge._1, edge._2))
                    kruskal(graph +| edge, seen + edge, n + 1)
                else {
                    kruskal(graph, seen + edge, n)
                }
            }
        }
        kruskal(this.withoutEdge)
    }

    /* COLORING METHODS */

    /** Sequence of vertices sorted by decreasing degree */
    lazy val sortedVertices : Seq[V] = vertices.toSeq.sortBy(degreeOf)(Ordering.Option[Int].reverse)

    /** Proper coloring using greedy algorithm (a.k.a WELSH-POWELL) */
    lazy val greedyColoring : Map[V, Int] = {
        //Déterminer le première couleur possible pour le sommet courant
        def firstAvailableColor(colors: Set[Int], unusedColors: Set[Int]): (Set[Int], Int) = {
            if (unusedColors.nonEmpty)
                (colors, unusedColors.head)
            else
                (colors + colors.size, colors.size)
        }

        @tailrec
        def treatAllVertices(vertices: Seq[V], colors: Set[Int] = Set.empty[Int], value: Map[V, Int] = Map.empty[V, Int]): Map[V, Int] ={
            if (vertices.isEmpty)
                value
            else {
                val v = vertices.head

                @tailrec
                def treatNeighbours(neighbors: Set[V], unusedColors: Set[Int] = colors): Set[Int] = {
                    if (neighbors.isEmpty)
                        unusedColors
                    else
                        treatNeighbours(neighbors.drop(1), {
                            val neighbor = neighbors.head
                            if (value contains neighbor)
                                unusedColors - value(neighbor)
                            else
                                unusedColors
                        })
                }

                val res = firstAvailableColor(value.values.toSet, treatNeighbours(neighborsOf(v).get))
                treatAllVertices(vertices.drop(1), res._1, value + (v -> res._2))
            }
        }

        treatAllVertices(sortedVertices)
    }

    /** Proper coloring using DSATUR algorithm */
    lazy val coloringDSATUR : Map[V, Int] = {
        //Déterminer le première couleur possible pour le sommet courant
        def firstAvailableColor(colors: Set[Int], usedColors: Set[Int]): Int = {
            val unusedColors = colors diff usedColors
            if (unusedColors.nonEmpty)
                unusedColors.head
            else
                colors.size
        }

        @tailrec
        def treatAllVertices(vertices: Set[V], value: Map[V, Int] = Map.empty[V, Int]): Map[V, Int] = {
            //Construire l'ensemble des voisins coloré d'un sommet
            @tailrec
            def neighborsColored(neighbors: Set[V], usedColors: Set[Int] = Set.empty[Int], result: Set[V] = Set.empty[V]): (Set[Int], Set[V]) = {
                if (neighbors.isEmpty)
                    (usedColors, result)
                else {
                    val neighbor = neighbors.head
                    if (value contains neighbor)
                        neighborsColored(neighbors.drop(1), usedColors + value(neighbor), result + neighbor)
                    else
                        neighborsColored(neighbors.drop(1), usedColors, result)
                }
            }

            //Définir le saturation degree of a vertex
            def saturationDegree(v: V): Int = {
                val nColored = neighborsColored(neighborsOf(v).get)._2
                if (nColored.isEmpty)
                    degreeOf(v).get
                else
                    nColored.size
            }

            //Traiter tous les sommets
            if (vertices.isEmpty)
                value
            else {
                val v = vertices maxBy saturationDegree
                val res = neighborsColored(neighborsOf(v).get)
                treatAllVertices(vertices - v, value + (v -> {

                    firstAvailableColor(value.values.toSet, res._1)
                }))
            }
        }
        treatAllVertices(this.vertices)
    }

    /* toString-LIKE METHODS */

    /** @inheritdoc */
    override lazy val toString : String = s"({${vertices mkString ", "}}, {${edges mkString ", "}})"

    /** Graph representation in DOT language */
    lazy val toDOTString : String = {
        "strict graph {\n" +
          "    // Edges\n" +
          (edges foldLeft "    ") {
              _ + _.toDOTString + "\n    "
          } + "\n" +
          "    // Vertices\n" +
          vertices.mkString("    ", "\n    ", "\n") +
          "  }\n"
    }
}
