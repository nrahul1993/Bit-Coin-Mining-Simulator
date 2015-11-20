package Local
import akka.routing._
import akka.remote._
import akka.remote.routing.RemoteRouterConfig
import akka.routing.RoundRobinRouter
import akka.actor._
import akka.actor.{ Address, AddressFromURIString }
import akka.actor.Actor._
import scala.util.control._
import scala.concurrent.duration._
import java.io._
import java.security.MessageDigest
import java.security.SecureRandom 
object Local extends App {
var cores= Runtime.getRuntime().availableProcessors()
	var w= cores*3/2  
case object CalculateLocal  
case object CalculateClient
case class mes(msg: String, duration: Duration)  
//System.setOut(new PrintStream(new FileOutputStream("output.doc")))
val system = ActorSystem("PiSystem")
var nrOfMessages=100000
var leadingZeroes=3
val master = system.actorOf(Props(new Master(nrOfMessages, listener)),name = "master")
val listener = system.actorOf(Props[Listener], name="listener")
master!"START CLIENT"
for (i <- 0 until nrOfMessages)
	master ! CalculateLocal
	class Master(nrOfEements:Int, listener: ActorRef) extends Actor {

	val start: Long=System.currentTimeMillis
			val workerRouter = context.actorOf( Props[Worker].withRouter(RoundRobinRouter(w)), name = "workerRouter")
			val remoteActor= context.actorFor("akka://HelloRemoteSystem@192.168.0.23:5150/user/Worker")
			var UFString="stalukdar"
			def randomString(length: Int): String = {
		val r = new scala.util.Random
				val sb = new StringBuilder
				for (i <- 1 to length) {
					sb.append(r.nextPrintableChar)
				}
		sb.toString
} 
def receive = {
  case "START CLIENT" =>
    remoteActor!"PING SERVER"
case "START" =>
for (i <- 0 until nrOfMessages)
	
  sender!UFString
case CalculateLocal =>
var randomUFString= UFString+randomString(10)
workerRouter!randomUFString
case msg:String =>
if(msg!="")
	listener ! mes("BitCoin Found =>"+msg, duration = (System.currentTimeMillis-start).millis)
}
}
class Listener extends Actor
{
	def receive=
		{
		case mes(msg,duration) =>
		println(msg)
		println(duration)
		

		}
}
class Worker extends Actor {

	def calculatePiFor(text: String, nrOfElements: Int): String = 
		{
			val m = java.security.MessageDigest.getInstance("SHA-256").digest(text.getBytes("UTF-8"))
					val hash = m.map("%02x".format(_)).mkString //SHA256(text)
					var bytearr=hash.getBytes()  //converting String to byte array
					var No_of_zeroes=0
					//Loop to calculate number of zeroes in the starting of the bitcoin
					val loop = new Breaks;
			loop.breakable
			{
				for (x<- bytearr)
				{
					if(x=='0')
						No_of_zeroes+=1
						else
							loop.break
				}
			}
			//Printing bitcoins starting with 3 zeroes and corresponding input string
			if(No_of_zeroes==leadingZeroes)
				text  + "   :  "+hash+"\n"
				else
					""
		}

	def receive = 
		{
		case msg:String =>
		var x =calculatePiFor(msg, 1000000 )
		sender ! x // perform the work
		}
}
}