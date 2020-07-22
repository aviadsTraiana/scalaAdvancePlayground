package exercises

class LazyMonad[+A](x: ⇒ A) {

  def evaluate = x
  def flatMap[B](f: (⇒A) ⇒ LazyMonad[B]):LazyMonad[B] = f(x)
}

object LazyMonad{
  def apply[A](x: ⇒ A): LazyMonad[A] = new LazyMonad[A](x)
}

object TestLaziness extends App{
  val lazyMonad= LazyMonad{
    println("side effect")
    42
  }
  println("no evaluation until now")
  println(lazyMonad.evaluate)
  val lazy2=lazyMonad.flatMap(x⇒ LazyMonad{x*2})
  println("no evaluation until now")
  lazy2.evaluate


}