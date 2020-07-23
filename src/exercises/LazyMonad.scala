package exercises

class LazyMonad[+A](x: ⇒ A) {
  private lazy val xByNeed = x
  def evaluate = xByNeed
  def flatMap[B](f: (⇒ A) ⇒ LazyMonad[B]): LazyMonad[B] = f(xByNeed)
  def map[B](f: A ⇒ B): LazyMonad[B] = flatMap(x ⇒ LazyMonad(f(x)))
  //def flatten[B](m: LazyMonad[LazyMonad[B]]) : LazyMonad[B] = m.flatMap( (x:LazyMonad[B]) ⇒ x)

}

object LazyMonad {
  def apply[A](x: ⇒ A): LazyMonad[A] = new LazyMonad[A](x)
}

object TestLaziness extends App {
  val lazyMonad = LazyMonad {
    println("side effect")
    42
  }
  println("no evaluation until now")
  println(lazyMonad.evaluate)
  println("------------------")
  val lazy2 = lazyMonad.flatMap(x ⇒ LazyMonad { x * 2 })
  val lazy3 = lazyMonad.flatMap(x ⇒ LazyMonad { x * 2 })

  println("no evaluation until now")
  println(lazy2.evaluate)
  println(lazy3.evaluate)

  //monads laws
//  val x = 5
//  def f(x:=>Int): LazyMonad[Int]  = LazyMonad(x+10)
//  def g(x:=>Int): LazyMonad[Int]  = LazyMonad(x*10)
//  //left
//  println(LazyMonad(x).flatMap(f) == f(x))
//  //right
//  println(LazyMonad(x).flatMap(x⇒ LazyMonad(x))==LazyMonad(x))
//  //associativity
//  println(LazyMonad(x).flatMap(f).flatMap(g) == LazyMonad(x).flatMap(x⇒ f(x).flatMap(g)))

}
