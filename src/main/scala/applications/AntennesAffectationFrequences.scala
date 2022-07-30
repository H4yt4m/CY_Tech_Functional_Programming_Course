package applications

import scala.collection.immutable.{IndexedSeq, Seq, Map}

class AntennesAffectationFrequences(CP: String, rayonDEmission: Double) extends Antennes {
  override def adjacencyMatrix(data: IndexedSeq[Map[String, Any]], adjacency: IndexedSeq[IndexedSeq[Int]], index: Int): IndexedSeq[IndexedSeq[Int]] = {
    if (index == data.size * data.size)
      adjacency
    else {
      adjacencyMatrix(data, {
        val i = index / data.size
        val j = index % data.size

        if (i != j) {
          val ithData = data(i)
          val jthData = data(j)

          if (distance(ithData("lat").toString.toInt, ithData("long").toString.toInt, jthData("lat").toString.toInt, jthData("long").toString.toInt) < 2 * rayonDEmission) {
            val newAdjacency = adjacency.updated(i, adjacency(i).updated(j, 1))
            newAdjacency.updated(j, newAdjacency(j).updated(i, 1))
          } else
            adjacency
        } else
          adjacency
      }, index + 1)
    }
  }

  override def run(): Unit = {
    val graph = getGraph(parseData(CP))

    println(s"\n\nVous allez affecter des fréquences aux antennes de la commune dont le code postal est ${CP}.")

    val algo = setting(
      Set("1", "2").contains,
      "\nVeuillez choisir un algorithme d'affectation de fréquence :\nWelsh-Powell : 1\nDTSATUR : 2\n",
      "Désolé, votre choix n'est valide."
    )

    if (algo == "1") {
      println("Affectation de fréquences selon l'algorithme de coloration de graphes de Wellsh-Powell.")
      println()
      graph.greedyColoring foreach { e =>
        println(s"L'antenne numéro ${e._1} est affectée à une fréquence de ${5000 * e._2 + 800} Hz.")
      }
    } else {
      println("Affectation de fréquences selon l'algorithme de coloration de graphes DSATUR.")
      println()
      graph.coloringDSATUR foreach { e =>
        println(s"L'antenne numéro ${e._1} est affectée à une fréquence de ${5000 * e._2 + 800} Hz.")
      }
    }
    println()
  }
}
