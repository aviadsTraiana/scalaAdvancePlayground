package exercises

import scala.annotation.tailrec

trait MySet[A] extends (A ⇒ Boolean ){

  def contains(e:A):Boolean

  def apply(e:A): Boolean = contains(e)

  def +(e:A):MySet[A]
  def ++(anotherSet:MySet[A]):MySet[A]

  def map[B](f: A⇒B):MySet[B]
  def flatMap[B](f: A⇒MySet[B]):MySet[B]
  def filter(pred: A ⇒ Boolean ) :MySet[A]
  def foreach(f:A ⇒ Unit):Unit

  def -(e:A):MySet[A]
  def --(anotherSet:MySet[A]):MySet[A] //difference
  def &(anotherSet:MySet[A]) : MySet[A] //intersection
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  override def contains(e: A): Boolean = false

  override def +(e: A): MySet[A] = new NonEmptySet[A](e,this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A ⇒ B): MySet[B] = new EmptySet[B]

  override def flatMap[B](f: A ⇒ MySet[B]): MySet[B] = new EmptySet[B]

  override def filter(pred: A ⇒ Boolean): MySet[A] = this

  override def foreach(f: A ⇒ Unit): Unit = {}

  override def -(e: A): MySet[A] = this

  override def --(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def &(anotherSet: MySet[A]): MySet[A] = this

  override def unary_! : MySet[A] = new AllInclusiveSet[A]
}

class NonEmptySet[A](head : A , tail:MySet[A]) extends MySet[A] {
  override def contains(e: A): Boolean = e==head || tail.contains(e)

  override def +(e: A): MySet[A] = if(this contains e) this else new NonEmptySet[A](e,this)

  override def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  override def map[B](f: A ⇒ B): MySet[B] = (tail map f)  + f(head)

  override def flatMap[B](f: A ⇒ MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  override def filter(pred: A ⇒ Boolean): MySet[A] = {
    val filteredTail = tail filter pred
    if(pred(head)) filteredTail + head
    else filteredTail
  }

  override def foreach(f: A ⇒ Unit): Unit = {
    f(head)
    tail foreach f
  }

  override def -(e: A): MySet[A] = this.filter(x⇒ x!=e)

  override def --(anotherSet: MySet[A]): MySet[A] = this.filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = this.filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x⇒ !this(x))
}

object MySet{

  def apply[A](values : A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc:MySet[A]) :MySet[A] =
      if(valSeq.isEmpty) acc
      else buildSet(valSeq.tail,acc + valSeq.head)
    buildSet(values.toSeq,new EmptySet[A])
  }
}


class AllInclusiveSet[A] extends MySet[A]{
  override def contains(e: A): Boolean = true

  override def +(e: A): MySet[A] = this

  override def ++(anotherSet: MySet[A]): MySet[A] = this

  override def map[B](f: A ⇒ B): MySet[B] = ???

  override def flatMap[B](f: A ⇒ MySet[B]): MySet[B] = ???

  override def filter(pred: A ⇒ Boolean): MySet[A] = ???

  override def foreach(f: A ⇒ Unit): Unit = ???

  override def -(e: A): MySet[A] = this.filter(x⇒ x!=e)

  override def --(anotherSet: MySet[A]): MySet[A] = this.filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = this.filter(anotherSet)

  override def unary_! : MySet[A] = new EmptySet[A]
}

class PropertyBasedSet[A](property : A ⇒ Boolean) extends MySet[A]{
  override def contains(e: A): Boolean = property(e)

  //{ x in A | property(x)} + e = {x in A | property(x) || x=e}
  override def +(e: A): MySet[A] = new PropertyBasedSet[A](x ⇒ property(x) || x==e)

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x⇒ property(x) || anotherSet(x))

  def `can't do` = throw new IllegalArgumentException("Really deep rabbit hole!")
  override def map[B](f: A ⇒ B): MySet[B] = `can't do`

  override def flatMap[B](f: A ⇒ MySet[B]): MySet[B] = `can't do`

  override def foreach(f: A ⇒ Unit): Unit = `can't do`

  override def filter(pred: A ⇒ Boolean): MySet[A] = new PropertyBasedSet[A](x⇒ property(x) && pred(x))

  override def -(e: A): MySet[A] = this.filter(x⇒ x!=e)

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x⇒ !property(x))
}
object TestSet extends App{
  val s = MySet(1,2,3,4)
  s + 5 ++MySet(1,6,7) + 3 flatMap(x ⇒ MySet(x,10*x)) filter (_%2 ==0) foreach(println)

  val `N - {1,2,3,4}` = !s
  println(`N - {1,2,3,4}`(2))
  println(`N - {1,2,3,4}`(5))
  val negativeEven= `N - {1,2,3,4}`.filter(x⇒ x%2==0)
  println(negativeEven(2))
  println(negativeEven(5))
  println(negativeEven(6))
  val negativeEvenPlusFive= negativeEven+5
  println(negativeEvenPlusFive(2))
  println(negativeEvenPlusFive(5))
  println(negativeEvenPlusFive(6))

}
