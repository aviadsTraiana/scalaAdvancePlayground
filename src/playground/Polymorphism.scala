package playground

object Polymorphism {

  trait GeneralObject
  trait Vehicle extends GeneralObject
  class Car extends Vehicle
  class Jeep extends Car
  class MotorCycle extends Vehicle
  class Person extends GeneralObject

  object Example1 {

    class Parking[T](place: T)

    val `1.` = new Parking[MotorCycle](new MotorCycle) //Compile
    //val `2.` = new Parking[MotorCycle](new Car) //Does not compile
    val `3.` = new Parking[Car](new Jeep) //Compile- Jeep is a Car
    val `4.` = new Parking(new Jeep) //type inference
  }

  object Example2 {

    class Parking[T](place1: T,place2:T)
    val `5.` = new Parking(new Jeep,new Car) //type inference -nearest common supertype
  }

  object Example3{
    //upperbound
    class Parking[T <: Vehicle](place: T)
    val `6.`=new Parking(new Jeep)// Compile
    //val `7.`=new Parking(new Person) //Does not compile-do not conform to class Parking's type parameter bounds

  }
  object Example4{
    //upper bound and lower bound
    class Parking[T >: Jeep <: Vehicle](place: T)

  }

  object InVariance{
    class MyList[A]{
      def head :A = ???
    }
    def foo(list: MyList[Vehicle])  = ???
    //foo(new MyList[Car]) //Compile error!
  }

  object CoVariance{
    class MyList[+A]{
      def head :A = ???
    }
    def foo(list: MyList[Vehicle]): Vehicle = list.head
    val bar: Vehicle = foo(new MyList[Car])
  }

  object ContraVariance{
    class MyList[-A]
    def foo(list:MyList[Car]):Unit = ???
    foo(new MyList[Vehicle])

  }



}
