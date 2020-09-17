val x= Seq(1,2,3,4,5)
val f= (x:Int) => "Number "+ x


def format(list:Seq[Int])(formatter: Int=>String):String = list.map(formatter).mkString(sep=", ")

format(x)(f)
//And even we can fill the data , and later decide how to format!

val dataFilled =format(x) _
//... (later on that program)
dataFilled(f)
