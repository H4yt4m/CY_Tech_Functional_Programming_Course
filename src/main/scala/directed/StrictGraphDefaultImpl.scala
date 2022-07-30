package directed

/** Default implementation for [[StrictGraph]]
  * @param vertices vertex set
  * @param arcs arcs set
  * @tparam V type for vertices
  */
case class StrictGraphDefaultImpl[V](vertices : Set[V], arcs : Set[Arc[V]]) extends StrictGraph[V] {

  /** @inheritdoc */
  def successorsOf(v: V): Option[Set[V]] =
    if (!(vertices contains v)) None else Some(vertices filter {
      arcs contains Arc(v, _)
    })

  /** @inheritdoc */
  def +(v: V): StrictGraphDefaultImpl[V] =
    StrictGraphDefaultImpl(vertices + v, arcs)

  /** @inheritdoc */
  def -(v: V): StrictGraphDefaultImpl[V] =
    StrictGraphDefaultImpl(vertices - v, arcs filterNot { e => Set(e._1, e._2) contains v })

  /** @inheritdoc */
  def +|(e: Arc[V]): StrictGraphDefaultImpl[V] =
    StrictGraphDefaultImpl(vertices ++ Set(e._1, e._2), arcs + e)

  /** @inheritdoc */
  def -|(e: Arc[V]): StrictGraphDefaultImpl[V] =
    StrictGraphDefaultImpl(vertices, arcs - e)

  /** @inheritdoc */
  def withoutArc: StrictGraphDefaultImpl[V] =
    StrictGraphDefaultImpl(vertices, Set.empty[Arc[V]])

  /** @inheritdoc */
  def withAllArcs: StrictGraphDefaultImpl[V] =
    StrictGraphDefaultImpl(vertices, for (v1 <- vertices; v2 <- vertices) yield Arc(v1, v2))
}
