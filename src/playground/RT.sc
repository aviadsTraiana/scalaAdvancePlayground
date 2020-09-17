import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

def compute1() :Future[Int] = Future{println("side-effect1");1}
def compute2() :Future[Int] = Future{println("side-effect2");2}

val sum= for{
 a <- compute1()
 b <- compute2()
} yield a+b

println(sum)