import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}


def protectFromFailure[A](f: =>Future[A]) = {
  Future.fromTry(Try(f)).flatten
}

def y = Future{
  throw new RuntimeException("oops")
  "y"
}

protectFromFailure(y).onComplete {
  case Success(value) => println("will not get here")
  case Failure(exception) => println("we were able to protect from exception")
}




