/**
  * Created by xu on 06/06/2017.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x+sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => sys.error("Empty List")
        case Cons(x, xs) => drop(xs, n-1)
      }
  }

  def dropWhile[A](al: List[A], f: A => Boolean): List[A] = al match {
    case Nil => sys.error("Empty List")
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else al
    }
  }

  def apprend[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, apprend(t, a2))
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Empty List")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  }

  val lst1 = List(1, 2, 3, 4, 5)
  println(dropWhile(lst1, (x: Int) => x < 4))
}

object learnScala {
  def main(args: Array[String]): Unit = {
    List()
  }
}
