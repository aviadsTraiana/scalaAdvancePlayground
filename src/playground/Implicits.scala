package playground

import scala.annotation.tailrec

object Implicits extends App {

  val pair: (String, Int) = "Aviad" → 5

  case class Person(name: String) {
    def sayHi = println(s"Hi, my name is $name")
  }

  implicit def fromStringToPerson(st: String): Person = Person(st)

  "Aviad" sayHi // convert by compiler to fromStringToPerson("Aviad").sayHi

  //parameters implicits
  def inc(x: Int)(implicit amount: Int) = x + amount
  //inject amount in compile search context
  implicit val injectedAmount: Int = 5
  println(inc(2)) // will get injected with 5 as param, and will print 7

  case class Pet(name: String, age: Int)
  val pets = List(Pet("loly", 6), Pet("rex", 3), Pet("A", 2))

  //to inject to sorted method
  //implicit val sortByAge : Ordering[Pet] = Ordering.fromLessThan((a,b)⇒ a.age<b.age)

  //println(pets.sorted)

  /*
  Implicit scope:
  - local scope
  - imported scope
  - companions of all types involved in the method signature in ours example sorted[B >: A](implicit ord: Ordering[B]): C
        meaning:
        -C=List
        -Ordering
        - all the types involved : A or any supertype B
   */

  //best practice is to write it in object (and companion object if it is heavily used) and import, example
  object AgeOrdering {
    implicit val sortByAge: Ordering[Pet] =
      Ordering.fromLessThan((a, b) ⇒ a.age < b.age)
  }
  import AgeOrdering._
  println(pets.sorted)

  //type enrichment
  implicit class RicherInt(val value: Int) extends AnyVal {
    def isEven = value % 2 == 0

    def isOdd = !isEven

    def times(function: () ⇒ Unit): Unit = {
      def auxTimes(n: Int): Unit = {
        if (n <= 0) ()
        else {
          function()
          auxTimes(n - 1)
        }
      }
      auxTimes(value)
    }

    def*[T](list:List[T]):List[T]= {
      @tailrec
      def concatenate(n:Int,acc:List[T]):List[T] = {
        if(n<=0) acc
        else{
          val newAcc=acc++list
          concatenate(n-1,newAcc)
        }
      }
      concatenate(value,List())
    }

  }

  println(2.isEven)
  println(2 isOdd)
  3 times(()⇒print("hell yeah "))
  println()
  println(4 * List(1,2))

}
