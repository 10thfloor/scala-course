import scala.annotation.tailrec
import scala.math.abs

object Week1 extends App {

  Console.println("Here comes some maths!")
  Console.println(sqrt(20))
  Console.println(factorial(5))
  Console.println(pascal(4, 6))
  Console.println(balanced("()))((".toList))

  def sqrt(x: Double): Double = {

    def sqrIter(guess: Double) : Double =
      if(goodEnough(guess)) guess
      else sqrIter(improve(guess))

    def goodEnough(guess: Double) : Boolean =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) : Double =
      (guess + x / guess) / 2

    sqrIter(1.0)

  }

  def factorial(n:Int) :Int  = {
    def loop(acc:Int, n:Int) : Int =
      if(n == 0) acc else loop(acc * n, n -1)

    loop(1, n)
  }

  def pascal(column:Int, row: => Int): Int = column match {
    case 1 => 1
    case 2 => row
    case _ => row * (row - 1) / 2
  }


  def balanced(input: List[Char]) :Boolean = {

    @tailrec
    def check(input: List[Char], acc:Int = 0): Boolean = {
      if (input.isEmpty) acc == 0
      else if (input.head == '(') check(input.tail, acc + 1)
      else if (input.head == ')' && acc > 0) check(input.tail, acc - 1)
      else check(input.tail, acc)
    }

   check(input)
  }
}