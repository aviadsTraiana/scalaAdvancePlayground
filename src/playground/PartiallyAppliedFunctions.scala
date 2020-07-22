package playground

object PartiallyAppliedFunctions extends App {

  val numbers= List(Math.PI,Math.E,1,9.8,1.3e-12)
  def curriedFormatter(s:String)(d:Double) :String = s.format(d)
  val simpleFormatter = curriedFormatter("%4.2f") _ //ETA Expansion
  val seriousFormatter = curriedFormatter("%8.6f") _
  val preciseFormatter = curriedFormatter("%14.12f") _
  println(numbers.map(preciseFormatter))
}
