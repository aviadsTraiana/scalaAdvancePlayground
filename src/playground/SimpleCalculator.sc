import Calculator.expPattern

import java.lang.Character.isDigit


object Calculator {
  final case class Error(m:String) extends AnyVal
  def apply(exp: String) :Either[Error,Int] =
    validate(exp).map(createTokens).map(calcTokens)

  private val expPattern =  raw"((\d+)(\+|\*)(\d+))*((\*|\+)(\d))?".r

  def validate(exp:String) :Either[Error,String] = {
    val results = expPattern.findAllIn(exp).toList
    if(results.size == 1) Right(exp)
    else Left(Error("invalid expression"))
  }

  sealed trait Token
  final case class Operator(char: Char) extends Token {
    def apply(x: Int, y: Int) = char match {
      case '*' => x * y
      case '+' => x + y
    }
  }
  final case class Number(value: Int) extends Token

  def calcTokens(tokens: Vector[Token]): Int = {
    var transformedTokens = tokens
    def calcOp(op: Char): Unit = {
      var i = 0
      while (i < transformedTokens.length) {
        transformedTokens(i) match {
          case operator @ Operator(o) if o == op =>
            val num1 = transformedTokens(i - 1).asInstanceOf[Number]
            val num2 = transformedTokens(i + 1).asInstanceOf[Number]
            val result = Number(operator.apply(num1.value, num2.value))
            transformedTokens = transformedTokens.patch(from = i - 1, patch = Vector(result), replaced = 3)
            i = i - 2
          case _ => //do nothing
        }
        i = i + 1
      }
    }
    calcOp('*')
    calcOp('+')
    transformedTokens.head.asInstanceOf[Number].value
  }
  def createTokens(exp: String): Vector[Token] = {
    var i = 0
    var tokens = Vector.empty[Token]
    var tokenBuffer = new StringBuffer
    while (i < exp.length) {
      val c = exp.charAt(i)
      if (isDigit(c)) tokenBuffer.append(c)
      else {
        val x = Integer.parseInt(tokenBuffer.toString)
        tokens = tokens :+ Number(x)
        tokens = tokens :+ Operator(c)
        tokenBuffer = new StringBuffer
      }
      i = i + 1
    }
    tokens = tokens :+ Number(Integer.parseInt(tokenBuffer.toString))
    tokens
  }
}

val exp = "12+2*3+5*2"
Calculator(exp)


