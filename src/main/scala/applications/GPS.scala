package applications

import directed.{Arc, StrictGraphMatrixImpl}

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.collection.immutable.IndexedSeq
import scala.io.StdIn

class GPS() extends Application {
  private final val verticesSequence = Seq("E", "H1", "H2", "E1", "E2", "A1", "A2", "I1", "I2", "A601", "A602")

  private final val adjacencyMatrix = IndexedSeq(
    IndexedSeq(0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
    IndexedSeq(0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0),
    IndexedSeq(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    IndexedSeq(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    IndexedSeq(1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    IndexedSeq(1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0),
    IndexedSeq(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1),
    IndexedSeq(0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0),
    IndexedSeq(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0)
  )

  //pondération des arcs par Temps
  private final val pondeTemps = Map(
    // E
    Arc("E", "E2") -> 0.5,
    Arc("E", "A1") -> 0.5,
    Arc("E", "A2") -> 0.5,

    // H1
    Arc("H1", "E1") -> 3.0,
    Arc("H1", "A1") -> 5.0,
    Arc("H1", "I2") -> 1.0,

    // H2
    Arc("H2", "A2") -> 5.0,
    Arc("H2", "I1") -> 1.0,

    // E1
    Arc("E1", "E") -> 0.5,

    // E2
    Arc("E2", "H2") -> 3.0,

    // A1
    Arc("A1", "E") -> 0.5,
    Arc("A1", "H1") -> 5.0,

    // A2
    Arc("A2", "E") -> 0.5,
    Arc("A2", "H2") -> 5.0,

    // I1
    Arc("I1", "H1") -> 1.0,
    Arc("I1", "A602") -> 0.1,

    // I2
    Arc("I2", "H2") -> 1.0,
    Arc("I2", "A601") -> 0.1,

    // A601
    Arc("A601", "I2") -> 0.1,

    // A602
    Arc("A602", "I1") -> 0.1,
  )

  //Pondération des arcs par distance
  private final val pondeDist = Map(
    // E
    Arc("E", "E2") -> 5.0,
    Arc("E", "A1") -> 5.0,
    Arc("E", "A2") -> 5.0,

    // H1
    Arc("H1", "E1") -> 100.0,
    Arc("H1", "A1") -> 2.0,
    Arc("H1", "I2") -> 10.0,

    // H2
    Arc("H2", "A2") -> 2.0,
    Arc("H2", "I1") -> 10.0,

    // E1
    Arc("E1", "E") -> 5.0,

    // E2
    Arc("E2", "H2") -> 100.0,

    // A1
    Arc("A1", "E") -> 5.0,
    Arc("A1", "H1") -> 2.0,

    // A2
    Arc("A2", "E") -> 5.0,
    Arc("A2", "H2") -> 2.0,

    // I1
    Arc("I1", "H1") -> 10.0,
    Arc("I1", "A602") -> 2.0,

    // I2
    Arc("I2", "H2") -> 10.0,
    Arc("I2", "A601") -> 2.0,

    // A601
    Arc("A601", "I2") -> 2.0,

    // A602
    Arc("A602", "I1") -> 2.0,
  )

  override def run(): Unit = {
    val graph = StrictGraphMatrixImpl(verticesSequence, adjacencyMatrix)

    println("\n\nVous êtes dans un bâtiment fictif et un GPS vous aidera à vous déplacer.")
    println("\nVous pouvez vous rendre à :\nE : Entrée\nE1 : Escalier 1\nE2 : Escalier 2\nA1 : Ascenseur 1\nA2 : Ascensseur 2\nH1 : Hall 1\nH2 : Hall 2\nI1 : Intersection 1\nI2 : Intersection 2\nA601 : Une salle du 6ème étage\nA602 : Une autre salle du 6ème étage")

    val start = setting(
      verticesSequence.contains,
      "\nVeuillez Entrer votre point de départ :\n",
      "\nDésolé, votre point de départ n'est pas défini."
    )

    val end = setting(
      verticesSequence.contains,
      "\nVeuillez Entrer votre point d'arrivée :\n",
      "\nDésolé, votre point d'arrivée n'est pas défini."
    )

    val param = setting(
      Set("c", "r").contains,
      "\nSouhaitez-vous le trajet le plus court, ou le plus rapide ? : [c/r]\n",
      "\nDésolé, le paramètre du trajet [c/r] n'est pas défini."
    )

    val path = graph.shortestPath({
      if (param == "c")
        pondeDist
      else
        pondeTemps
    })(start, end)

    println({
      if (path.isDefined) {
        s"Votre temps de trajet est estimé à ${ path.get._2 } ${{
          if (param == "c") "mètres" else "minutes"
        }} :\n${ path.get._1 mkString " -> " }."
      }
      else
        s"Désolé, il n'existe pas de chemin reliant ${ start } et ${ end }."
    })
    println()
  }
}
