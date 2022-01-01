import playground.ForComprehension.c
import playground.Implicits.pair
import playground.PatternMatching.r

import javax.swing.tree.TreeNode
import scala.:+
import scala.collection.mutable
//
//object ShuffleStringSolution {
//    def restoreString(s: String, indices: Array[Int]): String = {
//        val res = new Array[Char](indices.length)
//        (0 until s.length).foreach{ i=>
//          res.update(indices(i), s(i))
//        }
//        res.mkString
//    }
//}
//
//println(ShuffleStringSolution.restoreString("codeleet", Array(4,5,6,7,0,2,1,3)))
//

//Serialize and Deserialize N ary Tree
//class TreeNode(val value: Int,val size:Int = 0) {
//  val children = new Array[TreeNode](size)
//}
//
//object Codec {
//  val delimiter = ","
//  val nil = "_"
//  def serialize(root: TreeNode): String = {
//    def serTree(root: TreeNode, result: StringBuilder): Unit = {
//      if (root == null) result.append(nil).append(delimiter)
//      else {
//        result.append(s"${root.value}").append(delimiter)
//        val size = root.children.length
//        result.append(s"$size").append(delimiter)
//        root.children.foreach(node => serTree(node, result))
//      }
//    }
//    val result = new StringBuilder
//    serTree(root, result)
//    result.toString()
//  }
//
//  def deserialize(s: String): TreeNode = {
//    val nodes = s.split(delimiter)
//    def desTree(nodes:Array[String],index:Int) : TreeNode = {
//        val value = nodes(index)
//        if(value == nil) null
//        else {
//            val size = nodes(index+1).toInt
//            val root = new TreeNode(value.toInt,size)
//            (0 until size).foreach(i => root.children(i) = desTree(nodes,index+2))
//            root
//        }
//    }
//    desTree(nodes,0)
//  }
//
//  def print(root:TreeNode) : Unit = {
//        if(root!=null){
//          println(root.value)
//          root.children.foreach(n => print(n))
//        }
//    }
//
//}
//
//val root = new TreeNode(1,3)
//(0 until root.size).foreach(i=> root.children.update(i,new TreeNode(i+2)))
//val s = Codec.serialize(root)
//val original = Codec.deserialize(s)
//Codec.print(root)

object IntersectionSolution {
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    nums1.intersect(nums2).distinct
  }
}

//object findComplementSolution {
//  def findComplement(num: Int): Int = {
//    var n = num
//    val result = new StringBuilder
//    while (n > 0) {
//      if (n % 2 == 0) result.append("1") else result.append("0")
//      n = n / 2
//    }
//    val bits = result.toString().reverse
//    n = 0
//    (0 until bits.length).foreach { i =>
//      val bit = s"${bits(i)}".toInt
//      val power = math.pow(2, i).toInt
//      n = n + (bit * power)
//    }
//    n
//  }
//}
//findComplementSolution.findComplement(5)
//        val res = new Array[Char](indices.length)
//        (0 until s.length).foreach{ i=>
//          res.update(indices(i), s(i))
//        }
//        res.mkString
//    }
//}
//
//println(ShuffleStringSolution.restoreString("codeleet", Array(4,5,6,7,0,2,1,3)))
//

//Serialize and Deserialize N ary Tree
class TreeNode(val value: Int,val size:Int = 0) {
  val children = new Array[TreeNode](size)
}

object Codec {
  val delimiter = ","
  val nil = "_"
  def serialize(root: TreeNode): String = {
    def serTree(root: TreeNode, result: StringBuilder): Unit = {
      if (root == null) result.append(nil).append(delimiter)
      else {
        result.append(s"${root.value}").append(delimiter)
        val size = root.children.length
        result.append(s"$size").append(delimiter)
        root.children.foreach(node => serTree(node, result))
      }
    }
    val result = new StringBuilder
    serTree(root, result)
    result.toString()
  }

  def deserialize(s: String): TreeNode = {
    val nodes = s.split(delimiter)
    def desTree(nodes:Array[String],index:Int) : TreeNode = {
        val value = nodes(index)
        if(value == nil) null
        else {
            val size = nodes(index+1).toInt
            val root = new TreeNode(value.toInt,size)
            (0 until size).foreach(i => root.children(i) = desTree(nodes,index+2))
            root
        }
    }
    desTree(nodes,0)
  }

  def print(root:TreeNode) : Unit = {
        if(root!=null){
          println(root.value)
          root.children.foreach(n => print(n))
        }
    }

}

val root = new TreeNode(1,3)
(0 until root.size).foreach(i=> root.children.update(i,new TreeNode(i+2)))
val s = Codec.serialize(root)
val original = Codec.deserialize(s)
Codec.print(root)

object IntersectionSolution {
  def intersection(nums1: Array[Int], nums2: Array[Int]): Array[Int] = {
    nums1.intersect(nums2).distinct
  }
}

object findComplementSolution {
  def findComplement(num: Int): Int = {
    var n = num
    val result = new StringBuilder
    while (n > 0) {
      if (n % 2 == 0) result.append("1") else result.append("0")
      n = n / 2
    }
    val bits =result.toString()
    println(bits)
      n = 0
      (0 until bits.length).foreach { i =>
        val bit = s"${bits(i)}".toInt
        val power = math.pow(2,i).toInt
        n = n + (bit * power)
      }
    n
  }
}
//findComplementSolution.findComplement(2)


object StringCompression2Solution {
  def compress(s:String) = {
    var count = 1
    val map: mutable.Map[Char, Int] = new mutable.HashMap[Char,Int]()
    (0 until s.length-1).foreach{ i =>
      val c1 = s(i)
      val c2 = s(i+1)
      if(c1==c2){
        count=count+1
      }else{
        map.put(c1,count)
        count = 1
      }
    }
    map.put(s(s.length-1),count)
    s.distinct.map{c=>
      val countOfC = map(c)
      if(countOfC>1) s"$c$countOfC"
      else s"$c"
    }.mkString("")
  }
  def getLengthOfOptimalCompression(s: String, k: Int): Int = {
    if(k==0) compress(s).length
    else {
       s.foldLeft(Int.MaxValue){(m,c) =>

        val currentResult = s.replaceAll(s"$c","")
        val compressed = compress(currentResult).length
        Math.min(Math.min(m,compressed),getLengthOfOptimalCompression(currentResult,k-1))
      }
    }
  }
}

StringCompression2Solution.getLengthOfOptimalCompression("aaabcccd",2)