package applications

import scala.collection.immutable.{IndexedSeq, Map, Seq}
import undirected.{Edge, SimpleGraphMatrixImpl}

import scala.annotation.tailrec

class AntennesReconnexion(CP: String) extends Antennes {
  @tailrec
  private def valuation(data: IndexedSeq[Map[String, Any]], index : Int = 0, value: Map[Edge[String], Double] = Map.empty[Edge[String], Double]): Map[Edge[String], Double]  = {
    if (index == data.size * data.size)
      value
    else
      valuation(data, index + 1, {
        val i = index / data.size
        val j = index % data.size

        if (i > j) {
          val ithData = data(i)
          val jthData = data(j)
          value + (
            Edge(ithData("id").toString, jthData("id").toString) ->
              distance(ithData("lat").toString.toInt, ithData("long").toString.toInt, jthData("lat").toString.toInt, jthData("long").toString.toInt)
            )
        } else
          value
      })
  }

  override protected def adjacencyMatrix(data: IndexedSeq[Map[String, Any]], adjacency: IndexedSeq[IndexedSeq[Int]], index: Int): IndexedSeq[IndexedSeq[Int]] = adjacency

  override def run(): Unit = {
    val data = parseData(CP)
    val graph = getGraph(data).withAllEdges

    val values = valuation(data)
    val tree = graph.minimumSpanningTree(values)

    val length = tree.value(values filter { e => tree.edges contains e._1 })
    println(s"\n\nVous allez devoir reconnecter les antennes de la commune dont le code postal est ${CP}.")
    println(s"\nLa reconnexion optimale nécessite ${length} km de câbles et correspond au graphe suivant :\n\n${tree}")
    println()
  }
}
