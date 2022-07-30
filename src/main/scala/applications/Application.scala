package applications

import scala.io.StdIn

trait Application {
  protected def setting(testFunc: String => Boolean, question: String, errorMessage: String): String = {
    val res = StdIn.readLine(question)
    if (testFunc(res))
      return res
    else
      println(errorMessage)
      setting(testFunc, question, errorMessage)
  }

  def run(): Unit
}
