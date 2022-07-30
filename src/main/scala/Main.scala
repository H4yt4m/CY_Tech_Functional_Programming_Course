import applications.{Antennes, AntennesAffectationFrequences, AntennesReconnexion, GPS, Makefile}
import directed.StrictGraphSuccessorsImpl
import undirected.{SimpleGraphMatrixImpl, SimpleGraphNeighborsImpl}

import scala.io.Source.fromFile

object Main {
  def main(argv: Array[String]): Unit = {
    
    // new AntennesAffectationFrequences("65101", 0.1).run()
    // new AntennesReconnexion("65101").run()
    // new Makefile().run()
    new GPS().run()
  }
}