package sb

import scala.:+


object Spark extends App{

    //      1
    //2     3      4
    //     5 6   7 8 9
    trait Formatter[T] {
        def serialize(tree: T): String
        def deserialize(input: String): Option[T]
    }


    final case class Tree(value: Int, children: List[Tree])
    final case class ValueSize(value:Int,size:Int)
    final case class Index(var value:Int){
      def inc(): Unit = { value = value + 1}
    }
    object Tree {

        private val delimiter = ","
        private val nil = "#"

        def serialize(tree: Tree): String = {
            def serTree(tree:Tree,strBuilder:StringBuilder):Unit ={
                Option(tree).foreach{ tree=>
                  strBuilder.append(tree.value).append(delimiter).append(tree.children.size).append(delimiter)
                  tree.children.foreach(ch=> serTree(ch,strBuilder))
                }
            }
            val result = new StringBuilder
            serTree(tree,result)
            result.result()
        }
        def deserialize(input: String): Option[Tree] = {
            def buildTree(nodes:Vector[ValueSize],idx:Index):Tree= {
                if(idx.value==nodes.length) {
                  null
                }else {
                  val head = nodes(idx.value)
                      var root = Tree(head.value,List.empty[Tree])
                      (0 until head.size).foreach{ _ =>
                        idx.inc()
                        val subTree = buildTree(nodes,idx)
                        root = root.copy(children = root.children :+ subTree)
                      }
                  root
                }

            }
            val nodes =
                input
                    .init
                    .split(delimiter)
                    .sliding(size = 2,step = 2)
                    .map(arr=> ValueSize(arr(0).toInt,arr(1).toInt))
                    .toVector
           Option(buildTree(nodes,Index(0)))
        }

    }
  val `5` = Tree(5,List.empty[Tree])
  val `6` = Tree(6,List.empty[Tree])
  val `7` = Tree(7,List.empty[Tree])
  val `8` = Tree(8,List.empty[Tree])
  val `9` = Tree(9,List.empty[Tree])

  val `2` = Tree(2,List.empty[Tree])

  val `3` = Tree(3,List(`5`,`6`))
  val `4` = Tree(4,List(`7`,`8`,`9`))

  //      1
  //2     3      4
  //     5 6   7 8 9

  val tree =  Tree(1,List[Tree](`2`,`3`,`4`))
  println(tree)
  println(Tree.serialize(tree).init) //init return the list without the last element
  println(Tree.deserialize(Tree.serialize(tree)).get)
}
