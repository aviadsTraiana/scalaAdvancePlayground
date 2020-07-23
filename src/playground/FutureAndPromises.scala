package playground

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration._
object FutureAndPromises extends App {
  type UserName = String
  type Sender = UserName
  type Receiver = UserName
  type ItemName = String
  type Status = String

  def calculateMeaningOfLife() = {
    Thread.sleep(2000)
    42
  }

  def aFuture = Future {
    calculateMeaningOfLife()
  } //(global context)

  /*
  println(aFuture.value) // prints None:Option[Try[Int]]
  println("waiting on the future")
  aFuture
    .onComplete { //used for side effects, can be handled with Partial functions for Try[T]
      case Success(meaningOfLife) ⇒
        println(s"the meaning of life is $meaningOfLife")
      case Failure(exception) ⇒ println(s"I failed with exception: $exception")
    } //some thread running the pf,maybe thread that is not the same thread as of the future
  Thread.sleep(3000)
   */

  case class User(name: UserName)
  case class Transaction(s: Sender, r: Receiver, amount: Double, status: Status)
  object BankingApp {
    val name = "Leumi"
    def fetchUser(n: UserName): Future[User] = Future {
      // long computation
      Thread.sleep(500)
      User(n)
    }
    def createTransaction(user: User,
                          merchant: Receiver,
                          amount: Double): Future[Transaction] = Future {
      //simulate computation
      Thread.sleep(1000)
      Transaction(user.name, merchant, amount, "SUCCESS")
    }

    def purchase(n: UserName,
                 item: ItemName,
                 merchant: UserName,
                 cost: Double): Status = {
      //fetch user from DB
      // create transaction
      //WAIT FOR THE TRANSACTION TO FINISH
      val transactionStatus: Future[Status] = for {
        user ← fetchUser(n)
        transaction ← createTransaction(user, merchant, cost)
      } yield transaction.status

      //result blocks until there is a result and decompose it
      // ready blocks and wrap the result with Try
      Await.result(transactionStatus, atMost = 2.seconds)
    }
  }

  println(BankingApp
    .purchase("Aviad", "27InchMonitor", "Hightech-Zone", cost = 3000)) //SUCCESS

  //promises
  val promise = Promise[Int]()
  val future = promise.future
  //consumer
  future.onComplete(t ⇒
    t match {
      case Failure(exception) ⇒ exception.printStackTrace()
      case Success(value) ⇒ println("consumed " + value)
  })

  val producer = new Thread(() ⇒ {
    println("producer produce a magical number")
    Thread.sleep(500)
    promise.success(42)
    println("producer done")
  })

  producer.start()
  Thread.sleep(1000)

  case class FutureUtility[A](fa: Future[A]) {

    def -->:[B](second: Future[B]): Future[B] = fa.flatMap(_ ⇒ second)

    def completeFirst(fb: Future[A]): Future[A] = {
      val promise = Promise[A]
      fa.onComplete(promise.tryComplete)
      fb.onComplete(promise.tryComplete)

      promise.future
    }
    def completeLast(fb: Future[A]): Future[A] = {
      val bothPromise = Promise[A]
      val lastPromise = Promise[A]
      val checkAndComplete =
        (result: Try[A]) ⇒
          if (!bothPromise.tryComplete(result)) lastPromise.complete(result)
      fa.onComplete(checkAndComplete)
      fb.onComplete(checkAndComplete)

      lastPromise.future
    }
  }
  object FutureUtility {
    def immediateFuture[T](value: T): Future[T] = Future(value)

    def retryUntil[A](action: () ⇒ Future[A],condition: A⇒ Boolean): Future[A] ={
      action().filter(condition).recoverWith{
        case _ ⇒ retryUntil(action,condition)
      }
    }
  }
  val superFuture = FutureUtility(Future { println(5) })
  superFuture.-->:(Future { println(10) })

  val random = new Random()
  val randomAction: () ⇒ Future[Int] = () ⇒ Future[Int]{
    val nextValue = random.nextInt(100)
    println("generated "+nextValue)
    nextValue
  }

  val ru: Unit =FutureUtility.retryUntil(randomAction, (x:Int) ⇒ x<50).foreach(result ⇒ println("settled at "+ result))

}
