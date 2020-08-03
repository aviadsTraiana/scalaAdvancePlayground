package playground

object DuckTypingFeature extends App {

  type JavaClosable = java.io.Closeable

  class HipsterClosable{
    def close(): Unit = println("think I am closing something")
    def closeSilently():Unit = println("shh! closing")
  }

  //we would like to do something like:
  //def closeQuietly(closeable: JavaClosable OR HipsterClosable)

  type UnifiedClosable = { //STRUCTURAL TYPE
    def close():Unit
  }

  //now we can do duck-typing like:
  def closeQuietly(closeable: UnifiedClosable) = closeable.close()

  closeQuietly(new JavaClosable {
    override def close(): Unit = println("java closable")
  })
  closeQuietly(new HipsterClosable)

  //type refinements of JavaClosable
  type AdvancedClosable = JavaClosable {
      def closeSilently():Unit
  }

  def closeShh(advancedClosable: AdvancedClosable) :Unit = advancedClosable.closeSilently()
  //now when we pass an implementation of JavaClosable that also duck typing the closeSilently
  closeShh(new JavaClosable {
    override def close(): Unit = println("java closing")
    def closeSilently():Unit = println("shh! closing")
  })
  //but since we did not enrich HipsterClosable the following will not compile
  //closeShh(new HipsterClosable)

  //anonymous structural type:
  def alternativeClosable(closable:{ def close():Unit}):Unit = closable.close()

  alternativeClosable(new HipsterClosable)

  //static duck typing
  type SoundMaker ={
    def makeSound():Unit
  }
  class Dog {
    def makeSound():Unit = println("bark!")
  }
  class Car{
    def makeSound():Unit = println("vroom!")
  }

  val dog:SoundMaker = new Dog
  val car:SoundMaker =new Car
  dog.makeSound()
  car.makeSound()
/*
  warning: this is done by using reflection during runtime,
  also since it is done with reflection it is not perfectly type safe when using type-parameters. for example:
*/

  trait CBL[+T]  {
    def head: T
    def tail: CBL[T]
  }

  class Human {
    def head: Brain = new Brain
  }

  class Brain {
    override def toString: String = "BRAINZ!"
  }

  def f[T](somethingWithAHead: { def head: T }): Unit = println(somethingWithAHead.head)

  /*
    f is compatible with a CBL and with a Human? Yes.
   */

  case object CBNil extends CBL[Nothing] {
    def head: Nothing = ???
    def tail: CBL[Nothing] = ???
  }
  case class CBCons[T](override val head: T, override val tail: CBL[T]) extends CBL[T]

  f(CBCons(2, CBNil))
  f(new  Human) // ?! T = Brain , Cool :)

  // 2.
  object HeadEqualizer {
    type Headable[T] = { def head: T }

    def ===[T](a: Headable[T], b: Headable[T]): Boolean = a.head == b.head //reminder this is done with reflection
  }

  /*
    is compatible with a CBL and with a Human? Yes.
   */
  val brainzList = CBCons(new Brain, CBNil)
  val stringsList = CBCons("Brainz", CBNil)

  HeadEqualizer.===(brainzList, new Human)
  // problem:
  HeadEqualizer.===(new Human, stringsList) // not type safe


}
