package pf


object ProofPoint extends App {
  // Array(1,2,3,3,-4) , k = 6
  // 5,4,3,3,10

  final case class Review(doc:String)
  type Counter = Int
  type Token = String
  def f(reviews:Seq[Review],posWords:Seq[String]):Seq[Review] = {
    ???
  }
  def hasSum(items:Seq[Int],k:Int):Option[(Int,Int)] = {
    var s = Set.empty[Int]
    var i =0
    var result:Option[(Int,Int)] = None
    while(i<items.length){
        val item = items(i)
        val complement = k-item
        if(s contains item){
            result = Some((complement,item))
        }else{
          s = s + complement
        }
      i=i+1
    }
    result
  }


}
