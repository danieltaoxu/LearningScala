/**
  * Created by xu on 09/06/2017.
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

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
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
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)//f(foldLeft(xs, z)(f), x)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: List[Double]) = ns match {
    case Cons(0.0, _) => foldLeft(ns, 0.0)(_ * _)
    case _ => foldRight(ns, 1.0)(_ * _)
  }

  def reverse[A](ns: List[A]): List[A] = {
    foldLeft(ns, Nil: List[A])((x, xs) => Cons(xs, x))
  }

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }

  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(a1, a2)((x, xs) => Cons(xs, x))
  }

  def concat[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil: List[A])(append)
  }

  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, addOne(xs))
  }

  def doubleToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => List[B]()
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def mapWithFoldRight[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((h, t) => Cons(f(h), t))
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => List()
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def flatMapWithConcat[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(mapWithFoldRight(as)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def reduceForSum(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, reduceForSum(t1, t2))
  }

  def zipWith[A, B](a: List[A], b: List[B])(f: (A, B) => _): List[_] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
  }

  def checkPrefix[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(hl, tl), Cons(hp, tp)) if hl == hp => println(hl) ;checkPrefix(tl, tp)
    case _ => false
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if checkPrefix(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

  val lst1: List[Int] = List(1, 2, 3, 4, 5)
  val lst2: List[Int] = List(6, 7)
  val lst3: List[Int] = List(1, 2, 3, 4, 5, 6)
  val lst4: List[Int] = List(1, 2)
  /*println(dropWhile(lst1, (x: Int) => x < 4))
  dropWhileGroupArg(lst1)(x => x < 4)
  println(foldRight(List(3,2,1), Nil: List[Int])(Cons(_,_)))
  println(length(lst1))
  println(reverse(lst1))
  println(appendViaFoldLeft(lst1, lst2))
  println(addOne(lst1))
  println(flatMap(List(1,2,3))(i => List(i,i)))
  println(reduceForSum(lst1, lst2))*/
  //println(hasSubsequence(lst1, lst2))
  println(hasSubsequence(lst1, lst3))
  //println(hasSubsequence(lst1, lst4))
}