package playground

object Variance extends App {

  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal

  //no variance
  class Cage[T]
  // type substitution of generics
  //covariance
  class CovCage[+T]
  val cCage:CovCage[Animal] = new CovCage[Cat]

  //contra-variance
  class ContraCage[-T]
  val contraCage :ContraCage[Cat] = new ContraCage[Animal]

  //variance of positions (fields)
  class CovarianceCage[+T](val animal: T) //covariance => able to read (val)
  //class ContraVarianceCage[-T](var animal: T) //contravariance => will not compile- covariance position

  // hence we cannot just add covariance, so we can use widening the type for example
  class MyList[+A]{
    def add[B >: A](element: B):MyList[B] = new MyList[B] //now we can add to covariance :)
  }
  val emptyList=new MyList[Cat]
  emptyList.add(new Cat)
  val moreAnimals= emptyList.add(new Dog) //widening the return type to MyList[Animal]
  // because this is the lowest upper bound of Cat that is common


  //contra-variance return types
  class PetShop[-T]{
    def get[S <: T](isItAPuppy:Boolean,defaultAnimal:S): S = defaultAnimal
  }
  val shop:PetShop[Dog] = new PetShop[Animal]
  // val evilCat = shop.get(true,new Cat)// will not compile

  class Ascii extends Dog
  val asciiDog= shop.get(isItAPuppy = true,new Ascii)

  /**
   * 1. Invariant, covariant, contravariant
   *   Parking[T](things: List[T]) {
   *     park(vehicle: T)
   *     impound(vehicles: List[T])
   *     checkVehicles(conditions: String): List[T]
   *   }
   *
   * 2. used someone else's API: IList[T]
   * 3. Parking = monad!
   *     - flatMap
   */
  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  class IList[T]

  class IParking[T](vehicles: List[T]) {
    def park(vehicle: T): IParking[T] = ???
    def impound(vehicles: List[T]): IParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???

    def flatMap[S](f: T => IParking[S]): IParking[S] = ???
  }

  class CParking[+T](vehicles: List[T]) {
    def park[S >: T](vehicle: S): CParking[S] = ???
    def impound[S >: T](vehicles: List[S]): CParking[S] = ???
    def checkVehicles(conditions: String): List[T] = ???

    def flatMap[S](f: T => CParking[S]): CParking[S] = ???
  }

  class XParking[-T](vehicles: List[T]) {
    def park(vehicle: T): XParking[T] = ???
    def impound(vehicles: List[T]): XParking[T] = ???
    def checkVehicles[S <: T](conditions: String): List[S] = ???

    def flatMap[R <: T, S](f: R ⇒ XParking[S]): XParking[S] = ??? // T=>XParking[S] will not compile, but Function1[T,S]
    // will ,since now T is contra in Function1 and add type restriction [R <: T, S]
  }

  /*
    Rule of thumb
    - use covariance = COLLECTION OF THINGS
    - use contravariance = GROUP OF ACTIONS

    in this case parking is group of actions, so contra is more suitable
   */



}
