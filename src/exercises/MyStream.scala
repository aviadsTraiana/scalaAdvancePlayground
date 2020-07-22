package exercises

import scala.annotation.tailrec

abstract class MyStream[+A] {
  def isEmpty:Boolean
  def head:A
  def tail:MyStream[A]

  def #::[B >: A](element: B) :MyStream[B]
  def ++[B >: A](element: ⇒MyStream[B]) :MyStream[B]

  def foreach(f: A ⇒ Unit):Unit
  def map[B](f:A⇒B):MyStream[B]
  def flatMap[B](f:A⇒MyStream[B]):MyStream[B]
  def filter(predicate: A⇒ Boolean):MyStream[A]

  def take(n:Int):MyStream[A]
  def takeAsList(n:Int):List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc:List[B]=Nil):List[B] =
    if(isEmpty) acc.reverse
    else tail.toList(head:: acc)

}

object EmptyStream extends MyStream[Nothing]{
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException

  override def tail: MyStream[Nothing] = throw new NoSuchElementException

  override def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element,this)

  override def ++[B >: Nothing](anotherStream: ⇒ MyStream[B]): MyStream[B] = anotherStream

  override def foreach(f: Nothing ⇒ Unit): Unit = {}

  override def map[B](f: Nothing ⇒ B): MyStream[B] = this

  override def flatMap[B](f: Nothing ⇒ MyStream[B]): MyStream[B] = this

  override def take(n: Int): MyStream[Nothing] = this

  override def takeAsList(n: Int): List[Nothing] = Nil

  override def filter(predicate: Nothing ⇒ Boolean): MyStream[Nothing] = this
}

class Cons[+A](hd:A ,tl: ⇒MyStream[A]) extends MyStream[A]{
  override def isEmpty: Boolean = false

  override def head: A = hd

  override lazy val tail: MyStream[A] = tl

  override def #::[B >: A](element: B): MyStream[B] = new Cons[B](element,this)

  override def ++[B >: A](anotherStream: ⇒MyStream[B]): MyStream[B] = new Cons(head, tail ++ anotherStream)

  override def foreach(f: A ⇒ Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  override def map[B](f: A ⇒ B): MyStream[B] = new Cons[B](f(head),tail.map(f))

  override def flatMap[B](f: A ⇒ MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A ⇒ Boolean): MyStream[A] =
    if(predicate(head)) new Cons[A](head,tail.filter(predicate))
    else tail.filter(predicate)

  override def take(n: Int): MyStream[A] = {
    if(n<=0) EmptyStream
    else if(n==1) new Cons[A](head,EmptyStream)
    else new Cons[A](head,tail.take(n-1))
  }

}

object MyStream{
  def from[A](start:A)(generator:A⇒A):MyStream[A] ={
    new Cons[A](start,MyStream.from(generator(start))(generator))
  }
}

object StreamsPlayground extends App{
  val N_+ = MyStream.from(1)(_+ 1)
  println(N_+.head)
  println(N_+.tail.head)
  val N= 0 #::N_+
  println(N.head)

  N.take(500).map(_ * 2).foreach(println)
  println(N
    .flatMap(x⇒new Cons[Int](x, new Cons[Int](x+1,EmptyStream)))
    .take(10).filter(_ < 4).toList())


  def fibonacci(first:Int,second:Int):MyStream[Int]= new Cons[Int](first,fibonacci(second,first+second))
  println(fibonacci(1,1).take(10).toList())


 def getPrimes(n:Int): List[Int] = {
   def prime(numbers: MyStream[Int]): MyStream[Int] = {
     if (numbers.isEmpty) numbers
     else new Cons[Int](numbers.head, numbers.tail.filter(_ % numbers.head != 0))
   }
   prime(MyStream.from(2)(_ + 1)).take(n).toList()
 }

 println(getPrimes(100))
}
