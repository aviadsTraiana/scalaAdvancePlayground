package playground

import scala.concurrent.Future

object MagnetPattern extends App{

    //method overloading problem ( TypeErasure,Duplications)
    trait MessageMagnet[Result]{
      def apply(): Result
    }
    def someOverloadedMethod[R](magnet:MessageMagnet[R]) = magnet()

    //some overloaded method: String -> Int
    implicit class someParamOfOverLoadedMethod(param : String) extends MessageMagnet[Int]{
      override def apply(): Int = {
        println("the logic of the overloaded method")
        42
      }
    }
    implicit class someOtherParamOfOverLoadedMethod(otherParam : Int) extends MessageMagnet[Int]{
      override def apply(): Int = {
        println("the logic of the other overloaded method")
        24
      }
    }

    println(someOverloadedMethod("someString"))
    println(someOverloadedMethod(1))
    //benefits - no more type erasure Problem
    implicit class someGenericParamOfOverLoadedMethod(param : Future[String]) extends MessageMagnet[Int]{
      override def apply(): Int = 1
    }
    implicit class someOtherGenericParamOfOverLoadedMethod(otherParam : Future[Int]) extends MessageMagnet[Int]{
      override def apply(): Int = 2
    }
  import scala.concurrent.ExecutionContext.Implicits.global
  println(someOverloadedMethod(Future[String]{"Hi"}))
  println(someOverloadedMethod(Future[Int]{5}))
  //drawbacks - verbose , harder to read, can't use default arguments or name them, call by name does not work properly

}
