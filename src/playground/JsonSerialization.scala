package playground

import java.util.Date

object JsonSerialization extends App{
  case class User(name:String,age:Int,email:String)
  case class Post(content:String,createdAt:Date)
  case class Feed(user: User,posts:List[Post])

  sealed trait JsonValue{
    def stringify : String
  }

  //1 .intermediate data types - Int , String , Array , Object
  final case class JsonNumber(value:Int) extends JsonValue{
    override def stringify: String = String.valueOf(value)
  }
  final case class JsonString(value:String) extends JsonValue{
    override def stringify: String = "\""+value+"\""
  }
  final case class JsonArray(values:List[JsonValue]) extends JsonValue{
    override def stringify: String = values.map(_.stringify).mkString(sep = ",",start = "[",end = "]")
  }
  final case class JsonObject(values: Map[String,JsonValue]) extends JsonValue{
    override def stringify: String =
      values.map{ case (key,jsonValue) ⇒ "\""+ key +"\":"+jsonValue.stringify}
      .mkString(sep = ",",start = "{",end = "}")
  }

  /*
  Example:
  val data= JsonObject(Map(
    "user" → JsonString("Aviad"),
    "posts" → JsonArray(List(JsonString("Scala is cool"),JsonNumber(42)))
  ))
  println(data.stringify)
  */


  // 2. Type classes for conversion to intermediate data types
  //Type class
  trait JsonConverter[T]{
    def convert(value:T) : JsonValue
  }
  //Type instances:
  //existing data types
  implicit object StringConverter extends JsonConverter[String]{
    override def convert(value: String): JsonValue = JsonString(value)
  }
  implicit object IntConverter extends JsonConverter[Int]{
    override def convert(value: Int): JsonValue = JsonNumber(value)
  }

  //custom data types
  implicit object UserConverter extends JsonConverter[User]{
    override def convert(user: User): JsonValue = JsonObject(Map(
      "name" → JsonString(user.name),
      "age" → JsonNumber(user.age),
      "email" → JsonString(user.email)
    ))
  }
  implicit object PostConverter extends JsonConverter[Post]{
    override def convert(post: Post): JsonValue = JsonObject(Map(
      "content" → JsonString(post.content),
      "createdAt" → JsonString(post.createdAt.toString)
    ))
  }

  // 3. serialize to JSON
  implicit class JsonEnrichment[T](value:T){
    def toJson(implicit jsonConverter: JsonConverter[T]): JsonValue = jsonConverter.convert(value)
  }


  implicit object FeedConverter extends JsonConverter[Feed]{
    override def convert(feed: Feed): JsonValue = JsonObject(Map(
      "user" → feed.user.toJson,
      "posts"→ JsonArray(feed.posts.map(post ⇒ post.toJson))
    ))
  }


  /* Example:
  val now = new Date(System.currentTimeMillis())
  val aviad= User("aviad",30,"aviadshiber@gmail.com")
  val feed= Feed(aviad,List(
    Post("this is my first Post",now),
    Post("this is my second Post", now)
  ))
  println(feed.toJson.stringify)

   */

}
