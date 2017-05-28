package songday.server
import  scala.concurrent.duration._
import  scala.io.Source._
import  akka.pattern.ask
import  akka.util.Timeout
import  akka.actor._
import  spray.can.Http
import  spray.can.server.Stats
import  spray.util._
import  spray.http._
import  HttpMethods._
import  MediaTypes._
import  spray.can.Http.RegisterChunkHandler
import scala.util.matching.Regex
import MediaTypes._
import spray.json._
import DefaultJsonProtocol._
import scala.concurrent._
import java.io._
import scala.collection.mutable.Map
case class fileMsg(client: ActorRef, str: String)
case class userAuthMsg(client: ActorRef, user: UserAuthType)

class ResService extends Actor with ActorLogging {
  implicit val timeout: Timeout = 1.second // for the actor 'asks'
  import context.dispatcher // ExecutionContext for the futures and scheduler

  val reg = """/(.+)\.(js|txt|html|css|jpg|png|svg|woff|ttf)""".r
  val basePath = "/Users/grinch/idrive/html5/html5up-songday"

  def receive = {
    case x:fileMsg => {
      val client = x.client
      try {
      // var typeStr = ""
      val pathStr = basePath + x.str
      val reg(tmp, typeStr) = x.str
      println("Path is: " + pathStr + "\n" + typeStr + "\n\n")

      val sc = fromFile(pathStr, "iso-8859-1")
      if (!sc.isEmpty) {
        typeStr match {
          case "html" =>
          client ! HttpResponse(entity=HttpEntity(`text/html`, sc.mkString))
          case "css" =>
          client ! HttpResponse(entity=HttpEntity(`text/css`, sc.mkString))
          case "js" =>
          client ! HttpResponse(entity=HttpEntity(`application/javascript`, sc.mkString))
          case "jpg" =>
          client ! HttpResponse(entity=HttpEntity(`image/jpeg`, HttpData(new File(pathStr))))
          case "png" =>
          client ! HttpResponse(entity=HttpEntity(`image/png`, HttpData(new File(pathStr))))
          case "svg" =>
          client ! HttpResponse(entity=HttpEntity(`image/svg+xml`, HttpData(new File(pathStr))))
          
          case "woff" =>
          client ! HttpResponse(entity=HttpEntity(`application/font-woff`, 
            HttpData(new File(pathStr))))
          
          case "ttf" =>
          client ! HttpResponse(entity=HttpEntity(`application/x-font-truetype`, 
            HttpData(new File(pathStr))))

          case "ico" =>
          client ! HttpResponse(entity=HttpEntity(`image/x-icon`, HttpData(new File(pathStr))))

          case _ =>
          client ! HttpResponse(status = 404)
        }
      }
      else 
      client ! HttpResponse(status = 404)
      } catch {
        case _:Throwable => 
          client ! HttpResponse(status = 404)
      }
    }
  }
}

class AuthService extends Actor with ActorLogging {
import SongdayJsonProtocol._
  def receive = {
    case msg:userAuthMsg =>
      val client = msg.client
      val user = msg.user
      if (RedisController.checkUserPassword(user.username, user.password)) {
        TokenController.getToken(user.username) match {
          case Some(token) => 
            val auth = RedisController.getUserAuth(user.username) match {case Some(s) => s; case _ => ""};
            
            val data = LoginResponse(token, auth).toJson.toString
        
            client ! HttpResponse(entity=HttpEntity(`application/json`, 
              HttpData(data)))
            println("Auth Success!")
          case _ =>
            client ! HttpResponse(status=StatusCodes.Unauthorized, 
              entity=HttpEntity(`text/plain`, HttpData("Auth Failed, no name")))
            println("Auth Failed!")
        }
      } else {
        client ! HttpResponse(status=StatusCodes.Unauthorized, 
          entity=HttpEntity(`text/plain`, HttpData("uncorrect")))
        println("Auth Failed!")
      }
    case _ => None
  }
}

class DemoService extends Actor with ActorLogging {
  import SongdayJsonProtocol._

  implicit val timeout: Timeout = 1.second // for the actor 'asks'
  import context.dispatcher // ExecutionContext for the futures and scheduler
  
  // /* helpers */
  val reshdl = context.system.actorOf(Props[ResService])
  val authhdl = context.system.actorOf(Props[AuthService])
  // val authhdl = context.system.actorOf(Props[AuthService], name = "authHandler")
  
  def receive = {
    case _: Http.Connected => sender ! Http.Register(self)
    
    case req@HttpRequest(GET, Uri.Path("/"), _, _, _) =>
      val client = sender
      reshdl ! fileMsg(client, "/auth.html")
    
    case req@HttpRequest(POST, Uri.Path("/authenticate"), _, _, _) =>
      val user = req.entity.data.asString.parseJson.convertTo[UserAuthType]
      println(user)
      val client = sender
      authhdl ! userAuthMsg(client, user)

    case req@HttpRequest(POST, Uri.Path("/branchQuery"), _, _, _) => 
      println("data is: " + req.entity.data.asString)
      println("uri is: " + req.uri.path.toString)
      val client = sender
      val qr = req.entity.data.asString.parseJson.convertTo[SidTupleQueryFromClient]
      println("qr.token is:" + qr.token)
      println("qr.sidTuple is:" + qr.sidTuple)

      val tokencheck: Future[Boolean] = Future {
        // if (!TokenController.isTokenValidForAuth(qr.token, "Staff")) false

        val name = TokenController.getUserNameFromToken(qr.token)
        println("The name is:" + name)
        
        val userloc:String = RedisController.getUserWorkPlace(name) match {case Some(s) => s; case _ => ""};

        val userauth:String = (RedisController.getUserAuth(name) match {case Some(s) => s; case _ => ""})
        println("userloc is: "+userloc)
        println("userauth is: "+userauth)
        if (userloc != qr.sidTuple.location && userauth == "Staff")
            false
        else {
          println("Seems we can success")
          val data = SidTupleResponse(RedisController.queryStock(qr.sidTuple)).toJson.toString
          println("return data is:" + data)
          client ! HttpResponse(entity=HttpEntity(`application/json`, data))
          true
        }
      }

      tokencheck onSuccess {
        case true => 
        println("Finally, we get future results true")

        case false => 
        println("Finally, we get future results failed")
          client ! HttpResponse(status=StatusCodes.Unauthorized, 
              entity=HttpEntity(`text/plain`, HttpData("Auth Failed, no name")))
      }


    case req@HttpRequest(POST, Uri.Path("/branchAdjust"), _, _, _) =>
      println("data is: " + req.entity.data.asString)
      val client = sender
      val qr = req.entity.data.asString.parseJson.convertTo[SidTupleModifyFromClient]

      val tokencheck = Future {
        checkAuthForPath(qr.token, qr.sidTuple.location) match {
          case true => 
            RedisController.adjustStock(qr.sidTuple) match {
              case true =>
                client ! HttpResponse(entity=HttpEntity(`text/plain`, "done"))
              case _ =>
                client ! HttpResponse(entity=HttpEntity(`text/plain`, "uncomplete"))
            }
          case _ =>
            client ! HttpResponse(entity=HttpEntity(`text/plain`, "tokenFailed"))
        }
      }

      tokencheck onSuccess {
        case _ => None 
      }


    case req@HttpRequest(POST, Uri.Path("/recordAdd"), _, _, _) =>
      println("data is:" + req.entity.data.asString)
      val client = sender
      val qr = req.entity.data.asString.parseJson.convertTo[SaleRecordsModifyFromClient]

      val tokencheck = Future {
        checkAuthForPath(qr.token, qr.trancTuple.location) match {
          case true =>
          RedisController.addRecord(qr.trancTuple) match {
            case true =>
              client ! HttpResponse(entity=HttpEntity(`text/plain`, "done"))
            case false =>
              client ! HttpResponse(entity=HttpEntity(`text/plain`, "uncomplete"))
          }
          case false =>
            client ! HttpResponse(entity=HttpEntity(`text/plain`, "tokenFailed"))
        }
      }

      tokencheck onSuccess {
        case _ => None
      }



    case req@HttpRequest(POST, Uri.Path("/saleItem"), _, _, _) =>
      println("data is:" + req.entity.data.asString)
      val client = sender
      val qr = req.entity.data.asString.parseJson.convertTo[SaleRecordsModifyFromClient]

      val tokencheck = Future {
        checkAuthForPath(qr.token, qr.trancTuple.location) match {
          case true =>
            RedisController.saleItem(qr.trancTuple) match {
              case true =>
              client ! HttpResponse(entity=HttpEntity(`text/plain`, "done"))
              case false =>
              client ! HttpResponse(entity=HttpEntity(`text/plain`, "uncomplete"))
            }
          case false =>
            client ! HttpResponse(entity=HttpEntity(`text/plain`, "tokenFailed"))
        }
      }

      tokencheck onSuccess {
        case _ => None
      }




    case req@HttpRequest(POST, Uri.Path("/recordsQuery"), _, _, _) => 
      println("data is: " + req.entity.data.asString)
      println("uri is: " + req.uri.path.toString)
      val client = sender
      val qr = req.entity.data.asString.parseJson.convertTo[RecordsQueryFromClient]
      println("qr.token is:" + qr.token)
      println("qr.location is:" + qr.location)
      println("qr.ttype is:" + qr.ttype)

      val tokencheck : Future[Boolean] = Future {
        if (!TokenController.isTokenValidForAuth(qr.token, "Staff")) {
          false 
        } else {
          /* validate auth */
          val name = TokenController.getUserNameFromToken(qr.token)
          println("The name is:" + name)
        
          val userloc:String = RedisController.getUserWorkPlace(name) match {case Some(s) => s; case _ => ""};

          val userauth:String = (RedisController.getUserAuth(name) match {case Some(s) => s; case _ => ""})
          println("userloc is: "+userloc)
          println("userauth is: "+userauth)
          if (userloc != qr.location && userauth == "Staff") {
            false
          } else {
            /* auth check passed */
            println("Seems we can success")
            val data = SaleRecordsResponse(RedisController.queryRecords(Array(qr.location, qr.ttype, qr.user))).toJson.toString
            println("return data is:" + data)
            client ! HttpResponse(entity=HttpEntity(`application/json`, data))
            true
          }
        }

      }






    case req:HttpRequest => {
      val client = sender 
      req.uri.path.toString.contains("html") match {
        case true => 
          println("Come here!: " + req.uri.query.toString)
          val token = req.uri.query.get("token") match {
            case Some(tk) => tk
            case _ => ""
          }

          val res : Future[Boolean] = Future {
            println("the token is: " + token)
            TokenController.isTokenValidForAuth(token, "Staff")
          }
          res onSuccess {
            case true => 
              reshdl ! fileMsg(client, str=req.uri.path.toString)
            case false => 
              if (req.uri.path.toString.contains("html"))
                reshdl ! fileMsg(client, "/auth.html")
          }

        case false => 
          reshdl ! fileMsg(client, str=req.uri.path.toString)
      }
    }
    
  }

  def checkAuthForPath(token: String, location: String) : Boolean = {
    val name = TokenController.getUserNameFromToken(token)
    println("name is:" + name)
    val userauth = RedisController.getUserAuth(name) match {case Some(s) => s; case _ => None};
    println("userauth is: " + userauth)
    if (userauth == "Root" || userauth == "Mgr")
      return true
    if (location != RedisController.getUserWorkPlace(name))
      return false
    return true
  }

  class Streamer(client: ActorRef, count: Int) extends Actor with ActorLogging {
    log.debug("Starting streaming response ...")

    // we use the successful sending of a chunk as trigger for scheduling the next chunk
    client ! ChunkedResponseStart(HttpResponse(entity = " " * 2048)).withAck(Ok(count))

    def receive = {
      case Ok(0) =>
      log.info("Finalizing response stream ...")
      client ! MessageChunk("\nStopped...")
      client ! ChunkedMessageEnd
      context.stop(self)

      case Ok(remaining) =>
      log.info("Sending response chunk ...")
      context.system.scheduler.scheduleOnce(100 millis span) {
        client ! MessageChunk(DateTime.now.toIsoDateTimeString + ", ").withAck(Ok(remaining - 1))
      }

      case x: Http.ConnectionClosed =>
      log.info("Canceling response stream due to {} ...", x)
      context.stop(self)
    }

    // simple case class whose instances we use as send confirmation message for streaming chunks
    case class Ok(remaining: Int)
  }

}
