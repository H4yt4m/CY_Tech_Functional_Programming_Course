package directed

import scala.collection.immutable.Seq
import scala.collection.immutable.IndexedSeq
import scala.collection.immutable.Set
import scala.annotation.tailrec

/** Implementation of [[StrictGraph]] using adjacency matrix
  * @param vs
  *   sequence of vertices in the order they are used in adjacency matrix
  * @param adjacency
  *   adjacency matrix
  * @tparam V
  *   type for vertices
  */
case class StrictGraphMatrixImpl[V](vs: Seq[V], adjacency: IndexedSeq[IndexedSeq[Int]]) extends StrictGraph[V] {

  /** @inheritdoc */
  lazy val vertices: Set[V] =  vs.toSet

  /** @inheritdoc */
  lazy val arcs: Set[Arc[V]] = {
    @tailrec
    def createSetOfArcs(index: Int = 0, value: Set[Arc[V]] = Set.empty[Arc[V]]): Set[Arc[V]] = {
      if (index == vs.size * vs.size)
        value
      else
        createSetOfArcs(index + 1, {
          val i = index / vs.size
          val j = index % vs.size

          if (adjacency(i)(j) == 1)
            value + Arc(vs(i), vs(j))
          else
            value
        })
    }
    createSetOfArcs()
  }

  /** @inheritdoc */
  def successorsOf(v: V): Option[Set[V]] = {
    if (vs contains v)
      Some(arcs filter { _._1 == v } map { _._2 })
    else
      None
  }

  /** @inheritdoc */
  def +(v: V): StrictGraphMatrixImpl[V] = {
    if (!vertices.contains(v)) {
      StrictGraphMatrixImpl(vs :+ v, {
        val newAdjacency = (adjacency :+ IndexedSeq.fill(adjacency.size) { 0 }).transpose
        (newAdjacency :+ IndexedSeq.fill(newAdjacency(0).size) { 0 }).transpose
      })
    } else
      this
  }

  /** @inheritdoc */
  def -(v: V): StrictGraphMatrixImpl[V] = {
    if (vs contains v) {
      val i = vs indexOf v

      @tailrec
      def removeIthElement(oldIndexedSeq: IndexedSeq[IndexedSeq[Int]], newIndexedSeq: IndexedSeq[IndexedSeq[Int]] = IndexedSeq.empty[IndexedSeq[Int]], index: Int = 0): IndexedSeq[IndexedSeq[Int]] = {
        if (index == oldIndexedSeq.size)
          newIndexedSeq
        else
          removeIthElement(oldIndexedSeq, {
            if (index == i)
              newIndexedSeq
            else
              newIndexedSeq :+ oldIndexedSeq(index)
          }, index + 1)
      }

      StrictGraphMatrixImpl(vs filterNot (_ == v), {
        val newAdjacency = removeIthElement(adjacency).transpose
        removeIthElement(newAdjacency).transpose
      })
    }
    else
      this
  }

  /** @inheritdoc */
  def +|(e: Arc[V]): StrictGraphMatrixImpl[V] = {
    val newGraph = this + e._1 + e._2

    val i = newGraph.vs.indexOf(e._1)
    val j = newGraph.vs.indexOf(e._2)

    if (i != j && newGraph.adjacency(i)(j) == 0) {
       StrictGraphMatrixImpl(newGraph.vs, newGraph.adjacency.updated(i, newGraph.adjacency(i).updated(j, 1)))
    } else
       this
  }

  /** @inheritdoc */
  def -|(e: Arc[V]): StrictGraphMatrixImpl[V] = {
    if (!vs.contains(e._1) || !vs.contains(e._2))
      this
    else {
      val i = vs.indexOf(e._1)
      val j = vs.indexOf(e._2)

      if (adjacency(i)(j) == 1)
        StrictGraphMatrixImpl(vs, adjacency.updated(i, adjacency(i).updated(j, 0)))
      else
        this
    }
  }

  /** @inheritdoc */
  def withoutArc: StrictGraphMatrixImpl[V] = StrictGraphMatrixImpl(vs, IndexedSeq.fill(vs.size) { IndexedSeq.fill(vs.size) { 0 }})

  /** @inheritdoc */
  def withAllArcs: StrictGraphMatrixImpl[V] = {
    @tailrec
    def put0OnTheDiagonal(adjacency: IndexedSeq[IndexedSeq[Int]], i: Int = 0): IndexedSeq[IndexedSeq[Int]] = {
      if (i == vs.size)
        adjacency
      else
        put0OnTheDiagonal(adjacency.updated(i, adjacency(i).updated(i, 0)), i + 1)
    }
    StrictGraphMatrixImpl(vs, put0OnTheDiagonal(IndexedSeq.fill(vs.size) { IndexedSeq.fill(vs.size) { 1 }}))
  }
}