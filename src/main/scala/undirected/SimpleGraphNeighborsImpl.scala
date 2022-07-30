package undirected

import scala.collection.immutable.Set
import scala.collection.immutable.Map

/** Implementation of [[SimpleGraph]] using list of neighbors for each vertex
  *
  * @param neighbors associative map providing set of neighbors for each vertex
  *                  Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class SimpleGraphNeighborsImpl[V](neighbors : Map[V, Set[V]]) extends SimpleGraph[V] {

  /** @inheritdoc */
  val vertices : Set[V] = neighbors.keySet

  /** @inheritdoc */
  val edges : Set[Edge[V]] = neighbors.flatMap(e => e._2 map { x => Edge(e._1, x) }).toSet

  /** @inheritdoc */
  def neighborsOf(v: V) : Option[Set[V]] = neighbors.get(v)

  /** @inheritdoc */
  def +(v : V) : SimpleGraphNeighborsImpl[V] = {
    if (neighbors.contains(v))
      this
    else
      SimpleGraphNeighborsImpl[V](neighbors + (v -> Set.empty[V]))
  }

  /** @inheritdoc */
  def -(v : V) : SimpleGraphNeighborsImpl[V] = {
    if (neighbors.contains(v))
      SimpleGraphNeighborsImpl((neighbors - v) map { e => e._1 -> { if (e._2 contains v) e._2 - v else e._2 }})
    else
      this
  }

  /** @inheritdoc */
  def +|(e: Edge[V]): SimpleGraphNeighborsImpl[V] = {
    val newGraph = this + e._1 + e._2

    if (e._1 == e._2 || newGraph.neighbors(e._1).contains(e._2))
      this
    else
      SimpleGraphNeighborsImpl(newGraph.neighbors map { x => x._1 -> {
        val adjacent = e.adjacentTo(x._1)
        if (adjacent.isDefined) x._2 + adjacent.get else x._2
      }})
  }

  /** @inheritdoc */
  def -|(e: Edge[V]) : SimpleGraphNeighborsImpl[V] = {
    if (neighbors.contains(e._1) && neighbors.contains(e._2) && neighbors(e._1).contains(e._2))
      SimpleGraphNeighborsImpl(neighbors map { x => x._1 -> {
        val adjacent = e.adjacentTo(x._1)
        if (adjacent.isDefined) x._2 - adjacent.get else x._2
      }})
    else
      this
  }

  /** @inheritdoc */
  def withoutEdge: SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(neighbors map { _._1 -> Set.empty[V] })
  
  /** @inheritdoc */
  def withAllEdges: SimpleGraphNeighborsImpl[V] = SimpleGraphNeighborsImpl(neighbors map { e => e._1 -> (vertices - e._1)})
}
