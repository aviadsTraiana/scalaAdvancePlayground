package playground

object DarkSugar extends App{
  def singleArg(arg:Int) = "some function with single argument"

  val description= singleArg {
    // pass single arg here
    1
  }

  //scala spec: last char decides associativity of a method (colon means right)
  //example
  case class MyStream[T](){
    def -->: (value:T) : MyStream[T] = this
  }
  val myStream= 1-->:2-->: 3-->: MyStream[Int]()

  //multi world naming
  def `this is a function name`(): Unit = {}

  //infix types
//  class -->[A,B]
//
//  val infixExample: Int --> String = ???

  //update to set values
  val anArray=Array(1,2,3)
  anArray(1)=5 // syntax sugar for anArray.update(2,7)

  //backing getters and setters
  class GetterAndSetters{
    private var privateMember: Int = 0
    def member =privateMember //getter
    def member_=(newValue:Int):Unit = this.privateMember = newValue
  }
  val mutableMember= new GetterAndSetters
  mutableMember.member = 42
  println(mutableMember.member)
}
