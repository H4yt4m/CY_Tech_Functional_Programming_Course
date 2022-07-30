package undirected

/** Edge of an undirected graph as an unordered pair
  *
  * Source : [[https://gist.github.com/dmyersturnbull/2295d91ef503bb3eec6d]]
  * @param _1 one end of edge
  * @param _2 the other end of edge
  * @tparam V type of vertex
  */
case class Edge[V](_1 : V, _2 : V) {
    /** Get the other end of the edge
      * @param v one end
      * @return [[None]] if `v` is not an actual end, the other end otherwise
      */
    def adjacentTo(v : V) : Option[V] =
      if (v == _1) Some(_2) else if (v == _2) Some(_1) else None

    /** Edge representation in DOT language */
    lazy val toDOTString : String = s"${_1} -- ${_2}"

    /** @inheritdoc */
    override lazy val toString : String = toDOTString

    /* [equals] is redefined so Edge(a, b) == Edge(b, a) */
    /** @inheritdoc */
    override def equals(o : Any) : Boolean = o match {
        case that : Edge[V] => that._1 == _1 && that._2 == _2 || that._1 == _2 && that._2 == _1
        case _              => false
      }

    /* [hashCode] is redefined so Edge(a, b).hashCode == Edge(b, a).hashCode */
    /** @inheritdoc */
    override def hashCode : Int = _1.hashCode * _2.hashCode
  }
