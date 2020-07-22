package playground

object LazyEvaluation extends App {
  lazy val x: Int = {
    println("print only once")
    42
  }
  println(x)
  println(x)

  def callByName(n: ⇒ Int): Int = {
    lazy val t =n //call by need
    t+t+t+1
  }
  def longComputation ={
    println("loading...")
    Thread.sleep(1000)
    42
  }

  println(callByName(longComputation))
}
