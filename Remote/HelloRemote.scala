package remote
import akka.actor._
import akka.actor.Actor
import akka.routing.RoundRobinRouter
import scala.util.control._
import scala.Console
object HelloRemote extends App  {
var cores= Runtime.getRuntime().availableProcessors()
var leadingZeroes=3	
var w= cores*3/2  
	var bitcoinString=""
			var flag=0
case object CalculateLocal 
case class StringStore(str: String)
case class Result(value: String)
case class Work(text: String, nrOfElements: Int)
val system = ActorSystem("HelloRemoteSystem")
val masterActor = system.actorOf(Props[Worker], name = "Worker")
//masterActor!"PING SERVER"
class Worker extends Actor {
		val workerRouter = context.actorOf(Props[subWorker].withRouter(RoundRobinRouter(w)), name = "workerRouter")
				var UFString="stalukdar"
				def randomString(length: Int): String = {
			val r = new scala.util.Random
					val sb = new StringBuilder
					for (i <- 1 to length) {
						sb.append(r.nextPrintableChar)
					}
			sb.toString
		} 
		def receive = 
			{
			case "PING SERVER" =>
			println("Please Enter the IP of the server: ")
			var strr = Console.readLine()
			println(strr)
			println("CONNECTING SERVER")
			sender!"START"
			case msg:String =>
			var randomUFStringClient=msg+randomString(10)
			workerRouter!randomUFStringClient  //to subworker
			if(flag==1){
				sender !bitcoinString
			}
			flag=0	// perform the work

			case StringStore(str) =>
			if (str!="")
			{
				bitcoinString=str
						flag=1
			}
			}
	} 
	class subWorker extends Actor {
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
					text + "   :   "+hash+"\n"
					else
						""
			}

		def receive = 
			{
			case msg:String =>
			var x =calculatePiFor(msg, 1000000 )
			sender!StringStore(x)// remoteActor ! x // perform the work
			}
	}

}