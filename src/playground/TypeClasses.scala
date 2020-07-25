package playground



object TypeClasses extends App{
  case class User(name:String,age:Int){
    def sayHi = s"Hi my name is $name, and age is $age"
  }
  //type class
  trait Equal[T]{
    def apply(a:T,b:T): Boolean
  }
  //type class Instances
  implicit object NameEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }
  object FullEquality extends Equal[User]{
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.age==b.age
  }


  trait HTMLSerializer[T] {
    def serialize(value: T): String
    def check= true
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) </div>"
  }

  // 1 - we can define serializers for other  types
  import java.util.Date
  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString()}</div>"
  }

  // 2 - we can define MULTIPLE serializers
  object PartialUserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} </div>"
  }

  // part 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]): HTMLSerializer[T] = serializer //lift up the Type and give access to entire type class interface
  }
  implicit object IntSerializer  extends HTMLSerializer[Int]{
    override def serialize(value: Int): String = s"<div style='color=blue'>$value</div>"
  }

  println(HTMLSerializer.serialize(42))
  val aviad=User("aviad",30)
  println(HTMLSerializer.serialize(aviad))
  //give access to entire type class with apply
  println(HTMLSerializer[User].check)

  object Equal{ //companion obj to Equal trait
    def apply[T](a:T,b:T)(implicit equalizer:Equal[T]): Boolean = equalizer.apply(a,b)
  }

  println("injecting by compiler the NameEquality:")
  println(Equal(User("aviad",30),User("aviad",29)))

  implicit class HtmlEnricher[T](value:T){
    def toHTML(implicit serializer:HTMLSerializer[T]) = serializer.serialize(value)
  }

  println(aviad toHTML)

  implicit class EqualityEnrichment[T](value:T){
    def ===(anotherValue:T)(implicit equalizer:Equal[T]) = equalizer(value,anotherValue)
    def !==(anotherValue:T)(implicit equalizer:Equal[T]) = !(===(anotherValue))
  }

  println( aviad === User("aviad",29) )

  val injectedType: Equal[User] =implicitly[Equal[User]]
  println(injectedType)
}
