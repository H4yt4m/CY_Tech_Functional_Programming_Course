package undirected

/** Default implementation for [[SimpleGraph]]
  * @param vertices vertex set
  * @param edges edges set
  * @tparam V type for vertices
  */
case class SimpleGraphDefaultImpl[V](vertices : Set[V], edges : Set[Edge[V]]) extends SimpleGraph[V] {
    /** @inheritdoc */
    def neighborsOf(v: V): Option[Set[V]] =
      if (!(vertices contains v)) None else Some(vertices filter { edges contains Edge(v, _) })

    /** @inheritdoc */
    def + (v: V): SimpleGraphDefaultImpl[V] =
      SimpleGraphDefaultImpl(vertices + v, edges)

    /** @inheritdoc */
    def - (v: V): SimpleGraphDefaultImpl[V] =
      SimpleGraphDefaultImpl(vertices - v, edges filterNot { e => Set(e._1, e._2) contains v })

    /** @inheritdoc */
    def +| (e: Edge[V]): SimpleGraphDefaultImpl[V] =
      SimpleGraphDefaultImpl(vertices ++ Set(e._1, e._2), edges + e)

    /** @inheritdoc */
    def -| (e: Edge[V]): SimpleGraphDefaultImpl[V] =
      SimpleGraphDefaultImpl(vertices, edges - e)

    /** @inheritdoc */
    def withoutEdge: SimpleGraphDefaultImpl[V] =
      SimpleGraphDefaultImpl(vertices, Set.empty[Edge[V]])

    /** @inheritdoc */
    def withAllEdges: SimpleGraphDefaultImpl[V] =
      SimpleGraphDefaultImpl(vertices, for (v1 <- vertices ; v2 <- vertices) yield Edge(v1, v2))
}
