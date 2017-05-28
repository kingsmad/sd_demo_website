package songday.server
import scala.collection.mutable._
import com.redis._
import serialization._
import Parse.Implicits._
import spray.json._
import pdi.jwt._
import scala.util._
// case class Color(name: String, red: Int, green: Int, blue: Int)

// case class PersonType (Name: String, City: String, Country: String)
// case class Records (records: List[PersonType])

// object MyJsonProtocol extends DefaultJsonProtocol {
//    implicit val personTypeFormat = jsonFormat3(PersonType)
//   implicit val colorFormat = jsonFormat4(Color)
//   implicit val recordsFormat = jsonFormat1(Records)
// }

/* Sid tuples ===> (ttype, size, color, loc, number) */
case class SidTuple(ttype: String, size: String, color: String, location: String, number: String) 

case class SidTupleResponse(records: List[SidTuple])
case class SaleRecordsResponse(records: List[TrancTuple])
case class LoginResponse(token: String, auth: String)

object SidTuple {
  def apply(erno: Int): SidTuple = apply("-1", "-1", "-1", "-1", "-1")
}

/* Transaction tuples ===> (......) */
case class TrancTuple(ttype: String, size: String, color: String, location: String, 
  time: String, number: String, price: String, user: String)

case class TrancTupleRecords(records: List[TrancTuple])

object TrancTuple {
  def apply(erno: Int): TrancTuple = apply("1", "-1", "-1", "-1", "-1", "-1", "-1", "-1")
}

/* Json Supports */
case class UserInfo (name: String, authType: String)
case class UserAuthType (username: String, password: String)
case class Token (token: String)
case class SidTupleQueryFromClient (token: String, sidTuple: SidTuple)
case class RecordsQueryFromClient (token: String, user: String, location: String, 
  ttype: String)
case class SidTupleModifyFromClient(token: String, sidTuple: SidTuple)
case class SaleRecordsModifyFromClient(token: String, trancTuple: TrancTuple)

object SongdayJsonProtocol extends DefaultJsonProtocol {
  implicit val SidTupleFormat = jsonFormat5(SidTuple.apply)
  implicit val TrancTupleFormat = jsonFormat8(TrancTuple.apply)
  implicit val SidTupleResponseFormat = jsonFormat1(SidTupleResponse)
  implicit val TrancTupleRecordsFormat = jsonFormat1(TrancTupleRecords)
  implicit val UserInfoFormat = jsonFormat2(UserInfo)
  implicit val UserAuthTypeFormat = jsonFormat2(UserAuthType)
  implicit val TokenFormat = jsonFormat1(Token)
  implicit val SidTupleQueryFromClientFormat = jsonFormat2(SidTupleQueryFromClient)
  implicit val RecordsQueryFromClientFormat = jsonFormat4(RecordsQueryFromClient)
  implicit val SaleRecordsResponseFormat = jsonFormat1(SaleRecordsResponse)
  implicit val SidTupleModifyFromClientFormat = jsonFormat2(SidTupleModifyFromClient)
  implicit val SaleRecordsModifyFromClientFormat = jsonFormat2(SaleRecordsModifyFromClient)
  implicit val LoginResponseFormat = jsonFormat2(LoginResponse)
}

object RedisController {
  implicit def intToString(x: Int) = x.toString
  val redis = new RedisClient("127.0.0.1", 6379)

  /* Init DB */
  setUserAuth("liuyuan", "Mgr")
  setUserAuth("handeqin", "Staff")
  writeUserPassword("liuyuan", "123")
  writeUserPassword("handeqin", "124")
  setUserWorkPlace("liuyuan", "yjg")
  setUserWorkPlace("handeqin", "sm")


  redis.get[String]("avasid") match {
    case Some(s) => None
    case _ => redis.set("avasid", "0")
  }

  redis.get[String]("avatsid") match {
    case Some(s) => None
    case _ => redis.set("avatsid", "0")
  }

  /* MD5 */
  import java.security.MessageDigest
  def md5(s: String) = {
    MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02x".format(_)).mkString
  }

  /* Get SidTuple from sid */
  def getSidTupleFromSid(sid: String) :SidTuple = {
    if (!redis.exists(sid)) 
      return SidTuple(-1)
    redis.hmget[String, String](sid, "ttype", "size", 
      "color", "location", "number") match {
      case Some(smap) => SidTuple(smap("ttype"), smap("size"), 
        smap("color"), smap("location"), smap("number"))
      case _ => SidTuple(-1)
    }
  }

  /* Get Transaction record from its' tsid */
  def getTrancTupleFromTsid(tsid: String):TrancTuple = {
    if (!redis.exists("rec:"+tsid))
      return TrancTuple(-1)
    redis.hmget[String, String]("rec:"+tsid, "ttype", "size", "color", "location",
      "number", "price", "time", "user") match {
      case Some(smap) => TrancTuple(smap("ttype"), smap("size"), smap("color"),
        smap("location"), smap("time"), smap("number"), smap("price"), smap("user"))
      case _ => TrancTuple(-1)
    }
  }

  def adjustStock(ttype: String, size: String, color: String, location: String, delta: Int) : Boolean = {
    if (delta == 0) 
      return true
    val curTupleKey = ttype+":"+size+":"+color+":"+location
    redis.get[String](curTupleKey) match {
      case Some(sid) => {
        val n = redis.hmget[String, Int](sid, "number") match {
          case Some(mp) => mp("number")
          case _ => -1
        }
        if (n + delta < 0) return false
        redis.hincrby(sid, "number", delta)
        if (n + delta == 0) {
          redis.srem(ttype, sid)
          redis.srem(size, sid)
          redis.srem(color, sid)
          redis.srem(location, sid)
        }
        else if (n == 0) {
          redis.sadd(ttype, sid)
          redis.sadd(size, sid)
          redis.sadd(color, sid)
          redis.sadd(location, sid)
          redis.sadd("sidset", sid)
        }
      }
      case _ => {
        if (delta < 0) return false;
        redis.get[String]("avasid") match {
          case Some(sid) => 
            redis.incr("avasid")
            redis.set(curTupleKey, sid)
            redis.hmset(sid, Map("ttype" -> ttype, "size" -> size, "color"->color, 
              "location"->location, "number"->delta))
            redis.sadd(ttype, sid)
            redis.sadd(size, sid)
            redis.sadd(color, sid)
            redis.sadd(location, sid)
            redis.sadd("sidset", sid)

          case _ => return false
        }
      } 
    }
    return true
  }

  def adjustStock(sidtp: SidTuple) : Boolean = {
    if (sidtp.ttype != "null" && sidtp.size != "null" && 
      sidtp.color != "null" && sidtp.location != "null" &&
      sidtp.number != "null") {
      adjustStock(sidtp.ttype, sidtp.size, sidtp.color, sidtp.location, sidtp.number.toInt) 
    } else {
      false
    }
  }

  def addRecord(sidtp: SidTuple, time: String, price: Int, number: Int, user: String): Boolean = {
    redis.get[String]("avatsid") match {
      case Some(tsid) => 
        redis.incr("avatsid") 
        redis.hmset("rec:"+tsid, Map("time"-> time, "ttype"->sidtp.ttype, 
          "size"->sidtp.size, "color"->sidtp.color, "location"->sidtp.location, 
          "price"->price, "number"->number, "user"->user))
        redis.zadd("tsidzset", time.toDouble, tsid)
        redis.sadd("rec:"+sidtp.ttype, tsid)
        redis.sadd("rec:"+sidtp.location, tsid)
        redis.sadd("rec:"+user, tsid)
        true
      case _ => 
        println("getavatsid failed")
        false
    }
  }

  def addRecord(ttp: TrancTuple ) : Boolean = {
  //   (ttype: String, size: String, color: String, location: String, 
  // time: String, number: String, price: String, user: String)
    if (ttp.ttype == "null" || ttp.size == "null" || ttp.color == "null" ||
      ttp.location == "null" || ttp.time == "null" || ttp.number == "null" ||
      ttp.price == "null" || ttp.user == "null") {
      println("ttp is: " + ttp)
      false
    } 
    addRecord(SidTuple(ttp.ttype, ttp.size, ttp.color, ttp.location, -1), ttp.time, 
       ttp.price.toInt,ttp.number.toInt, ttp.user)
  }


  def saleItem(ttp: TrancTuple) : Boolean = {
    if (ttp.ttype == "null" || ttp.size == "null" || ttp.color == "null" ||
      ttp.location == "null" || ttp.time == "null" || ttp.number == "null" ||
      ttp.price == "null" || ttp.user == "null") {
      println("ttp is: " + ttp)
      false
    } 
    val stp = SidTuple(ttp.ttype, ttp.size, ttp.color, ttp.location, "-"+ttp.number)
    if (adjustStock(stp) == false)
      return false
    if (addRecord(stp, ttp.time, ttp.price.toInt, ttp.number.toInt, ttp.user) == false) {
      val stp = SidTuple(ttp.ttype, ttp.size, ttp.color, ttp.location, (-ttp.number.toInt).toString)
      adjustStock(stp)
    } else true
  }


  def queryStock(nlist: Array[String]):List[SidTuple] = {
    val ans = new ListBuffer[SidTuple]
    var mlist = nlist.filter((item:String) => (item != "null"))
        // if (mlist.isEmpty) {
    //   val mlist = List("sidset", "sidset")
    // }
    if (mlist.isEmpty) {
      mlist = Array("sidset")
    }
    // println("mlist is: ")
    // mlist.foreach ((x:String) => (println(x+", ")))

    redis.sinter[String](mlist(0), mlist:_*) match {
      case Some(s) => {
        for (item <- s) {
          item match {
            case Some(sid) => ans += getSidTupleFromSid(sid)
            case _ => None
          }
        }
      }
      case _ => None
    }
    return ans.toList
  }

  def queryStock(st: SidTuple) : List[SidTuple] = {
    println("querying......")
    println(st.ttype + "|" + st.color + "|" + st.size + "|" + st.location + "\n")
    queryStock(Array(st.ttype, st.color, st.size, st.location))
  }

  /* Only location and type is allowed here*/
  def queryRecords(nlist: Array[String]):List[TrancTuple] = {
    val ans = new ListBuffer[TrancTuple]
    // val mlist = nlist.filter((item: String) => (item != "null"))
    var mlist = for {s <- nlist.filter((item: String) => (item != "null"))} yield "rec:"+s
    
    println("mlist is: ")
    mlist.foreach((x:String) => (println(x+", ")))

    if (mlist.isEmpty) {
      redis.zrange[String]("tsidzset", 0, -1) match {
        case Some(s) => 
          for (item <- s) {
            // item match {
            //   case Some(tsid) => ans += getTrancTupleFromTsid(tsid)
            //   case _ => None
            // }
            ans += getTrancTupleFromTsid(item)
          }
        case _ => None
      }
      return ans.toList
    }

    redis.sinter[String](mlist(0), mlist:_*) match {
      case Some(s) => {
        for (item <- s) {
          item match {
            case Some(tsid) =>  ans += getTrancTupleFromTsid(tsid)
            case _ => None
          }
        }
      }
      case _ => None
    }
    return ans.toList
  }

  def writeUserPassword(name: String, passwd: String) {
    // redis.set("userpassword:"+name, md5(passwd))
    redis.hmset("userinfo:"+name, Map("password"->md5(passwd)))
  }

  def checkUserPassword(name: String, passwd: String): Boolean = {
    // redis.get[String]("userpassword:"+name) match {
    //   case Some(st) => st == md5(passwd)
    //   case _ => false
    // }
    redis.hmget[String, String]("userinfo:"+name, "password") match {
      case Some(smap) =>
        smap("password") == md5(passwd)
      case _ => false
    }
  }

  def setUserWorkPlace (name: String, wp: String) {
    redis.hmset("userinfo:"+name, Map("workplace"->wp))
  }

  def getUserWorkPlace(name: String) = {
    redis.hmget[String, String]("userinfo:"+name, "workplace") match {
      case Some(smap) =>
        Option(smap("workplace"))
      case _ => None
    }
  }

  private def getUserInfoAll(name: String) = {
    redis.hmget[String, String]("userinfo:"+name, "workplace",
     "auth", "password") 
  }

  private def setUserAuth(name: String, auth: String) {
    if (List("Root", "Mgr", "Staff").contains(auth)) {
      // redis.set("userauth:"+name, auth)
      redis.hmset("userinfo:"+name, Map("auth"->auth))
    }
  }
  def getUserAuth(name: String) : Option[String] = {
    // redis.get[String]("userauth:"+name)
    redis.hmget[String, String]("userinfo:"+name, "auth") match {
      case Some(smap) =>
        Option(smap("auth"))
      case _ => None
    }
  }
}

object TokenController {
  import SongdayJsonProtocol._

  val secretKey = "A_Person_Without_Regret_Is_A_Nincompoop"
  val exprTime = 600

  def userType(token: String) {

  }

  def checkpasswd(user: String, passwd: String) {

  }

  def changePasswd(user: String, passwd: String) {

  }

  def isTokenValidForAuth(token: String, userType: String) :Boolean = {
    Jwt.decode(token, secretKey, Seq(JwtAlgorithm.HS256)) match {
      case Success(tokenString) => 
        val user = tokenString.parseJson.convertTo[UserInfo]
        userType match {
          case "Root" => user.authType == "Root"
          case "Mgr" => user.authType == "Mgr" || user.authType == "Root"
          case "Staff" => true
        }
      case Failure(s) => false
    }
  }

  def getUserNameFromToken(token: String) : String = {
    Jwt.decode(token, secretKey, Seq(JwtAlgorithm.HS256)) match {
      case Success(tokenString) => 
        val user = tokenString.parseJson.convertTo[UserInfo]
        user.name
      case Failure(s) => ""
    }
  }

  private def getToken(name: String, auth: String, exprTime: Long) :String = {
    val str = "{\"name\":\"" + name + "\"," + "\"authType\":\"" + auth + "\"}" 
    Jwt.encode(JwtClaim(str).issuedNow.expiresIn(exprTime), secretKey, JwtAlgorithm.HS256)
  }

  def getToken(name: String, epTime: Long=exprTime) : Option[String] = {
    RedisController.getUserAuth(name) match {
      case Some(auth) => 
        Some(getToken(name, auth, epTime))
      case _ => None
    }
  }
}

object QueryController {

}