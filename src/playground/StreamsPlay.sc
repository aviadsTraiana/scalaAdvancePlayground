import scala.collection.immutable.HashMap
import scala.collection.mutable

def kDuplicationExist(array: Array[Int],k:Int) :Boolean = {
  array.foldLeft(new HashMap[Int, Int]){ (hashMap, a_i) =>
      val result:HashMap[Int,Int] = hashMap.get(a_i) match {
          case Some(v) =>  hashMap + (a_i -> (v+1))
          case None => hashMap + (a_i -> 1)
      }
    println(result)
    result
  }.exists( p => p._2 >= k )
}

def kDuplicationExist2(array: Array[Int],k:Int) :Boolean = {
  var counter = 0
  val hashMap = new mutable.TreeMap[Int,Int]()
  array.foldLeft(false){ (acc,a_i) =>
    counter= counter+1
    val nextValue = hashMap.getOrElse(a_i,0)+1
    hashMap.put(a_i,nextValue)
    if(nextValue>=k) true else acc
  }
}
kDuplicationExist2(Array(1,2,3,4,2,5,2,7),3)