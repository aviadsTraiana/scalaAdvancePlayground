package playground

object TypeSystem extends App {

  trait Writer[T] {
    def write(value:T) :Unit
  }
  trait Closable{
    def close(status:Int) : Unit
  }
  trait GenericStream[T]{
    def foreach(f:T ⇒ Unit)
  }
  //convenience feature
  def processStream[T](stream : GenericStream[T] with Writer[T] with Closable) ={
    stream.foreach(println)
    stream.close(0)
  }

  //diamond problem
  trait Animal { def name:String}
  trait Lion extends Animal {
    override def name: String = "Simba"
  }
  trait Tiger extends Animal {
    override def name: String = "Tiger"
  }
  class Cat extends Lion with Tiger // we don't have conflict here

  val cat =new Cat

  println(cat.name) //prints Tiger
  /**
   * Explanation:
   * Cat
   * extends Animal with { override def name:String = "Simba"}
   * <s> with Animal </s>  with { override def name:String ="Tiger" }
   *
   * so the compiler solve this conflict by choosing the last one
   */


  //------------------------------------------------

  //super type problem + type linearization
  trait Color{
    def print = println("color")
  }
  trait Green extends Color {
    override def print: Unit = {
      println("green")
      super.print
    }
  }
  trait Blue extends Color {
    override def print: Unit = {
      println("blue")
      super.print
    }
  }
  class Red{
    def print = println("red")
  }

  class White extends Red with Green with Blue{
    override def print: Unit = {
      println("white")
      super.print
    }
  }
  val color = new White
  color.print // will print white blue green color
  /**
   * Explanation
   * -----------
   * White = Red with Green with Blue = Anyref with <Red>
   *   with (<s>Anyref</s> with <Green>) with (<s>Anyref</s> with <Blue>) =Anyref with <Red> with <Green> with <Blue>
   *  each super will call the method of the lefty type
   */

}
