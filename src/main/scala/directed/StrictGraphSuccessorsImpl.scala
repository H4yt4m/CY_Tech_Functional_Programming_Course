package directed

import scala.collection.Set
import scala.collection.Map


/** Implementation of [[StrictGraph]] using list of successors for each vertex
  * @param successors associative map providing set of successors for each vertex
  *                   Key must be defined for any vertex in graph : should an actual vertex have no neighbor, value is defined and is an empty set
  * @tparam V type for vertices
  */
case class StrictGraphSuccessorsImpl[V](successors : Map[V, Set[V]]) extends StrictGraph[V] {

  /** @inheritdoc */
  val vertices : Set[V] = successors.keySet

  /** @inheritdoc */
  val arcs : Set[Arc[V]] = successors.flatMap(e => e._2 map { x => Arc(e._1, x) }).toSet

  /** @inheritdoc */
  def successorsOf(v: V) : Option[Set[V]] = successors get v

  /** @inheritdoc */
  def +(v : V) : StrictGraphSuccessorsImpl[V] = {
    if (successors.contains(v))
      this
    else
      StrictGraphSuccessorsImpl(successors + (v -> Set.empty[V]))
  }

  def -(v: V): StrictGraphSuccessorsImpl[V] = {
    if (successors.contains(v))
      StrictGraphSuccessorsImpl((successors - v) map { e => e._1 -> { if (e._2 contains v) e._2 - v else e._2 }})
    else
      this
  }

  /** @inheritdoc */
  def +|(e: Arc[V]) : StrictGraphSuccessorsImpl[V] = {
    val newGraph = this + e._1 + e._2

    if (e._1 == e._2 || newGraph.successors(e._1).contains(e._2))
      this
    else
      StrictGraphSuccessorsImpl(newGraph.successors map { x => x._1 -> { if (x._1 == e._1) x._2 + e._2 else x._2 }})
  }

  /** @inheritdoc */
  def -|(e: Arc[V]) : StrictGraphSuccessorsImpl[V] = {
    if (successors.contains(e._1) && successors.contains(e._2) && successors(e._1).contains(e._2))
      StrictGraphSuccessorsImpl(successors map { x => x._1 -> { if (x._1 == e._1) x._2 - e._2 else x._2 }})
    else
      this
  }

  /** @inheritdoc */
  def withoutArc : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(successors map { _._1 -> Set.empty[V] })

  /** @inheritdoc */
  def withAllArcs : StrictGraphSuccessorsImpl[V] = StrictGraphSuccessorsImpl(successors map { e => e._1 -> (vertices - e._1)})
}