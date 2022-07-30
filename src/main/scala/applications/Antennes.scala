package applications

import undirected.SimpleGraphMatrixImpl

import scala.collection.immutable.{IndexedSeq, Map, Seq}
import scala.io.Source.fromFile
import java.lang.Math.{PI, acos, cos, sin}
import scala.annotation.tailrec

trait Antennes extends Application {
  private val lines: Iterator[String] = {
    val L = fromFile("src/main/resources/antennes.csv").getLines()
    L.next()
    L
  }

  protected final def parseData(CPh: String, data: IndexedSeq[Map[String, Any]] = IndexedSeq.empty[Map[String, Any]]): IndexedSeq[Map[String, Any]] = {
    if (!lines.hasNext)
      data
    else {
      val cols = lines.next() split ";" map { _.trim }

      if (cols.length == 15 && cols(14) == CPh) {
        val id = cols(1) concat cols(2)

        val COR_NB_DG_LAT = cols(3).toInt
        val COR_NB_MN_LAT = cols(4).toInt
        val COR_NB_SC_LAT = cols(5).toInt
        val COR_CD_NS = cols(6)

        val COR_NB_DG_LONG = cols(7).toInt
        val COR_NB_MN_LONG = cols(8).toInt
        val COR_NB_SC_LONG = cols(9).toInt
        val COR_CD_EW = cols(10)

        val lat = {
          if (COR_CD_NS == "N") 1 else -1
        } * (3600 * COR_NB_DG_LAT + 60 * COR_NB_MN_LAT + COR_NB_SC_LAT)
        val long = {
          if (COR_CD_EW == "E") 1 else -1
        } * (3600 * COR_NB_DG_LONG + 60 * COR_NB_MN_LONG + COR_NB_SC_LONG)

        parseData(CPh, data :+ Map("id" -> id, "lat" -> lat, "long" -> long))
      } else
        parseData(CPh, data)
    }
  }

  private final def arcSecondToRadian(angle: Int): Double = angle * PI / 648000

  protected final def distance(lat1: Int, long1: Int, lat2: Int, long2: Int): Double =
    6378.137 * acos(sin(arcSecondToRadian(lat1)) * sin(arcSecondToRadian(lat2)) + cos(arcSecondToRadian(lat1)) * cos(arcSecondToRadian(lat2)) * cos(arcSecondToRadian(long2) - arcSecondToRadian(long1)))

  @tailrec
  private final def verticesSeq(data: IndexedSeq[Map[String, Any]], vertices: Seq[String] = Seq.empty[String]): Seq[String] = {
    if (data.isEmpty)
      vertices
    else
      verticesSeq(data.drop(1), vertices :+ data.head("id").toString)
  }

  protected final def getGraph(data: IndexedSeq[Map[String, Any]]): SimpleGraphMatrixImpl[String] = {
    val vertices = verticesSeq(data)
    SimpleGraphMatrixImpl(vertices, adjacencyMatrix(data, IndexedSeq.fill(vertices.size) { IndexedSeq.fill(vertices.size) { 0 }}))
  }

  protected def adjacencyMatrix(data: IndexedSeq[Map[String, Any]], adjacency: IndexedSeq[IndexedSeq[Int]], index: Int = 0): IndexedSeq[IndexedSeq[Int]]
}