package playground

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object OptFuture extends App {

  case class Data(value:Int)
  val x:Option[Data] = Option(null)

  val result =for{
   data <- x
  } yield {
    Future{
      Thread.sleep(2000)
      println(s"the result is:${data.value + 1}")
    }
  }
  Future {
    result.getOrElse(throw new RuntimeException("missing arg"))
    Thread.sleep(3000)
  }.onComplete {
    case Failure(exception) => println(exception.getMessage)
    case Success(_) => print("success :)")
  }
  Thread.sleep(10000)

}
