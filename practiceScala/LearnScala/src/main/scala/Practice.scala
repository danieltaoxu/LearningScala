/**
  * Created by tao on 04/06/2017.
  */
class Practice {

}


object myModule {
  def abs(n: Int) :Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int) : Int = {
      if (n <= 0) acc
      else go(n-1, n*acc)
    }
    go(n, 1)
  }

  def fib(n: Int) : Int = {
    @annotation.tailrec
    def tail_position_fib(n: Int, val_n_minus_one: Int, val_n_minus_two: Int) : Int = {
      if (n <= 1) val_n_minus_one
      else {
        tail_position_fib(n-1, val_n_minus_one+val_n_minus_two, val_n_minus_one)
      }
    }
    tail_position_fib(n, 1, 0)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def sort(low: Int, high: Int, isInOrder: Boolean) : Boolean = {
      if (low > high || !isInOrder) isInOrder && false
      else {
        val isOk = gt(as(low), as(high))
        if (high >= as.length - 1) isInOrder && isOk
        else {
          sort(low + 1, high + 1, isOk)
        }
      }
    }
    sort(0, 1, true)
  }

  def partiall[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def main(args: Array[String]) : Unit = {
    /*println(formatAbs(-42))
    println(factorial(3))
    println(fib(9))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    val lessThan = new ((Int, Int) => Boolean) {
      def apply(v1: Int, v2: Int): Boolean = v1 < v2
    }
    lessThan.apply(1,2)
    println(isSorted[Int](Array(1,3,2,5), (v1, v2) => v1 < v2))
    println(partiall[Int, Int, Boolean](1, (v1, v2) => v1 < v2))*/
    Tree.begin()
  }
}