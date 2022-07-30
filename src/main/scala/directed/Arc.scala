package directed

/** Arc of a directed graph as an ordered pair
  * @param _1 origin      of edge
  * @param _2 destination of edge
  * @tparam V type of vertex
  */
case class Arc[V](_1 : V, _2 : V) {
    /** Arc representation in DOT language */
    lazy val toDOTString : String = s"${_1} -> ${_2}"

    /** @inheritdoc */
    override lazy val toString : String = toDOTString
}
