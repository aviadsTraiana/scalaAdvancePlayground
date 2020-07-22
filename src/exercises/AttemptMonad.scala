package exercises

trait AttemptMonad[+A] {
  def flatMap[B](f:A ⇒ AttemptMonad[B]):AttemptMonad[B]
}

object AttemptMonad{
  def apply[A](x : ⇒ A): AttemptMonad[A] = try{ Success(x)} catch { case e: Throwable ⇒ Fail(e)}
}

case class  Success[+A](value:A) extends AttemptMonad[A]{
  override def flatMap[B](f: A ⇒ AttemptMonad[B]): AttemptMonad[B] = try { f(value) } catch { case e:Throwable ⇒ Fail(e)}
}

case class Fail(e: Throwable) extends AttemptMonad[Nothing]{
  override def flatMap[B](f: Nothing ⇒ AttemptMonad[B]): AttemptMonad[B] = this
}

object TestAttemptMonad extends App{
  /**
   * left identity
   * --------------
   * unit.flatMap(f) = f(x)   => for success case: Attempt(x).flatMap(f) = f(x) => Success(x).flatMap(f) = f(x)
   *
   * right identity
   * --------------
   * attempt.flatmap(unit) = attempt => success(x).flatMap(x=> attempt(x)) = attempt(x) = success(x)
   * in case of failure: Fail(e).flatMap(_) = Fail(e)
   *
   * associativity
   * -------------
   * attempt.flatmap(f).flatmap(g) = attempt.flatmap(x => f(x).flatmap(g))
   */

  val x= 5
  def f(x:Int) :AttemptMonad[Int]= AttemptMonad(x*2)
  def g(x:Int) :AttemptMonad[Int]= AttemptMonad(x*2)
  //left
  println(AttemptMonad(x).flatMap(f) == f(x))
  //right
  println(Success(x).flatMap(x⇒ AttemptMonad(x))==Success(x))
  //associativity
  println( AttemptMonad(x).flatMap(f).flatMap(g) == AttemptMonad(x).flatMap(x⇒ f(x).flatMap(g)) )

  println( AttemptMonad{ throw new RuntimeException("failed monad")})

}