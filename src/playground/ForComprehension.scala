package playground

object ForComprehension extends App {

  val c: Option[Int] = for {
    a <- Option(1)
    b <- Option(2) if b%2==0
  } yield a + b

  println(c)

}
