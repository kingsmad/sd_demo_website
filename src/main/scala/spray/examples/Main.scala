package songday.server

import scala.io.Source._
import scala.collection.mutable.Map
import akka.actor.{ActorSystem, Props}
import akka.io.IO
import spray.can.Http

case class ResourceHelper(base: String, resources: List[String]) {
  val resMap = Map[String, String]()
  for (rs <- resources) {
    val f = fromFile(base + rs)
    if (!f.isEmpty) {
      resMap += (rs -> f.mkString)
    }
  }

  def context (res: String): String = {
    println("the context input is: " + res)
    resMap get res match {
      case Some(s) => s
      case None =>
        val f = fromFile(base + res)
        if (f.isEmpty) return "This is isEmpty"
        else {
          resMap += (res -> f.mkString)
          // println("The context is: "+f.mkString)
          f.mkString
        }
    }
  }
}

object Main extends App with MySslConfiguration {

  implicit val system = ActorSystem()

  // the handler actor replies to incoming HttpRequests
  val handler = system.actorOf(Props[DemoService], name = "handler")

  IO(Http) ! Http.Bind(handler, interface = "localhost", port = 8080)
}
