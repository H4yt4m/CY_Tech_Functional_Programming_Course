package applications

import directed.StrictGraphSuccessorsImpl

import scala.collection.immutable.{Map, Set}
import scala.io.Source.fromFile
import scala.annotation.tailrec

class Makefile extends Application {
  private val data = fromFile("src/main/resources/Makefile").getLines() filterNot { _.isEmpty } mkString "\n"

  @tailrec
  private def createSuccessors(successors: Map[String, Set[String]] = Map(data.split(" : ")(0) -> Set.empty[String]), lines: Array[String] = data split "\n"): Map[String, Set[String]] = {
    if (lines.isEmpty)
      successors
    else {
      val lineSplit = lines.head split " : "
      val successor = lineSplit(0)
      val keys = lineSplit(1).split(" ")
      createSuccessors(successors ++ keys.flatMap(k => Map(k -> {
        if (successors contains k)
          successors(k) + successor
        else
          Set(successor)
      })), lines.drop(1))
    }
  }

  override def run(): Unit = {
    val graph = StrictGraphSuccessorsImpl(createSuccessors())
    val order = graph.topologicalOrder.get
    println("\n\nCompilation de votre projet C.")
    println(s"\nLes fichiers sont compilés dans l'ordre suivant :\n${order mkString " -> "} afin de respecter les dépendances.")
    println()
  }
}
