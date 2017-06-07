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

  def dropWhileGroupArg[A](al: List[A])(f: A => Boolean): List[A] = al match {
    case Nil => sys.error("Empty List")
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else al
    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))//foldRight(xs, f(x, z))(f)
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: List[Double]) = ns match {
    case Cons(0.0, _) => foldRight(ns, 0.0)(_ * _)
    case _ => foldRight(ns, 1.0)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((x, y) => y + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A)  => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) = ns match {
    case Cons(0.0, _) => foldLeft(ns, 0.0)(_ * _)
    case _ => foldRight(ns, 1.0)(_ * _)
  }

  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil: List[A])((x, xs) => Cons(xs, x))
  }

  val lst1: List[Int] = List(1, 2, 3, 4, 5)
  //println(dropWhile(lst1, (x: Int) => x < 4))
  dropWhileGroupArg(lst1)(x => x < 4)
  println(foldRight(List(3,2,1), Nil: List[Int])(Cons(_,_)))
  println(length(lst1))
  println(reverse(lst1))
}

object learnScala {
  def main(args: Array[String]): Unit = {
    List()
  }
}
