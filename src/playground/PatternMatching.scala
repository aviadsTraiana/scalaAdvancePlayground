package playground

/**
 * available for
 * - const
 * - wildcards
 * - case classes
 * - tuples
 * - "magic syntax"
 */
object PatternMatching extends App {
  val numbers= List(1)
  val firstNumber = numbers match {
    case head ::Nil ⇒ head
  }
  println(firstNumber+5)

  //decompose classes that cannot be data class
  class Person(val name:String,val age:Int)
  //with companion and unapply this can be decomposed
  object PersonPattern{
    def unapply(p: Person): Option[(String, Int)] = Some((p.name,p.age))
  }
  val bob= new Person("bob",30)
  val greeting = bob match {
    case PersonPattern(n,a) ⇒ s"my name is $n , my age $a"
  }
  println(greeting)

  val num=46
  object even{
    def unapply(x: Int): Boolean = x%2==0
  }
  object singleDigit{
    def unapply(x: Int): Boolean = x<10
  }
  val r = num match {
    case singleDigit() ⇒ "single digit"
    case even() ⇒ "even number"
    case _ ⇒ "not single or even"
  }
  println(r)

  case class Or[A,B](a:A,b:B)
  val either= Or(2,"two")
  val description = either match {
    case number Or string ⇒ s"$number or a $string"
  }
  println(description)

  val aPartialFunction :PartialFunction[Int,Int] = {
    case 1⇒ 42
    case 2⇒ 56
    case 3 ⇒ 999
  }
  println(aPartialFunction(2))
  //aPartialFunction(4) //will throw exception
  aPartialFunction.isDefinedAt(4) //return false
  //lift
  val lifted=aPartialFunction.lift
  println(lifted(1)) // Some(42)
  println(lifted(77)) //None
  val pfChain= aPartialFunction.orElse[Int,Int]{
    case 4 ⇒ 22
  }
  pfChain.isDefinedAt(4) //return true
  pfChain.isDefinedAt(1) //return true

  // PF extends normal function and can have only one type

}
