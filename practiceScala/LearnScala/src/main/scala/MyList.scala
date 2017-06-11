/**
  * Created by xu on 09/06/2017.
  */
sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x+sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def drop[A](l: MyList[A], n: Int): MyList[A] = {
    if (n <= 0) l
    else
      l match {
        case Nil => sys.error("Empty List")
        case Cons(x, xs) => drop(xs, n-1)
      }
  }

  def dropWhile[A](al: MyList[A], f: A => Boolean): MyList[A] = al match {
    case Nil => sys.error("Empty List")
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else al
    }
  }

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Nil => sys.error("Empty List")
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))

  }

  def dropWhileGroupArg[A](al: MyList[A])(f: A => Boolean): MyList[A] = al match {
    case Nil => sys.error("Empty List")
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else al
    }
  }

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))//foldRight(xs, f(x, z))(f)
  }

  def sum2(ns: MyList[Int]) = {
    foldRight(ns, 0)((x, y) => x + y)
  }

  def product2(ns: MyList[Double]) = ns match {
    case Cons(0.0, _) => foldRight(ns, 0.0)(_ * _)
    case _ => foldRight(ns, 1.0)(_ * _)
  }

  def length[A](as: MyList[A]): Int = {
    foldRight(as, 0)((x, y) => y + 1)
  }

  @annotation.tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A)  => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)//f(foldLeft(xs, z)(f), x)
  }

  def sum3(ns: MyList[Int]) = foldLeft(ns, 0)((x, y) => x + y)

  def product3(ns: MyList[Double]) = ns match {
    case Cons(0.0, _) => foldLeft(ns, 0.0)(_ * _)
    case _ => foldRight(ns, 1.0)(_ * _)
  }

  def reverse[A](ns: MyList[A]): MyList[A] = {
    foldLeft(ns, Nil: MyList[A])((x, xs) => Cons(xs, x))
  }

  def foldRightViaFoldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }

  def appendViaFoldRight[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
    foldRight(a1, a2)(Cons(_, _))
  }

  def appendViaFoldLeft[A](a1: MyList[A], a2: MyList[A]): MyList[A] = {
    foldLeft(a1, a2)((x, xs) => Cons(xs, x))
  }

  def concat[A](a1: MyList[MyList[A]]): MyList[A] = {
    foldRight(a1, Nil: MyList[A])(append)
  }

  def addOne(l: MyList[Int]): MyList[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x+1, addOne(xs))
  }

  def doubleToString(l: MyList[Double]): MyList[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, doubleToString(xs))
  }

  def map[A,B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case Nil => MyList[B]()
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def mapWithFoldRight[A,B](as: MyList[A])(f: A => B): MyList[B] = {
    foldRight(as, Nil: MyList[B])((h, t) => Cons(f(h), t))
  }

  def flatMap[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case Nil => MyList()
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def flatMapWithConcat[A,B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
    concat(mapWithFoldRight(as)(f))
  }

  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
    flatMap(as)(a => if (f(a)) MyList(a) else Nil)
  }

  def reduceForSum(a1: MyList[Int], a2: MyList[Int]): MyList[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, reduceForSum(t1, t2))
  }

  def zipWith[A, B](a: MyList[A], b: MyList[B])(f: (A, B) => _): MyList[_] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah, bh), zipWith(at, bt)(f))
  }

  def checkPrefix[A](l: MyList[A], prefix: MyList[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(hl, tl), Cons(hp, tp)) if hl == hp => println(hl) ;checkPrefix(tl, tp)
    case _ => false
  }

  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if checkPrefix(sup, sub) => true
    case Cons(h, t) => hasSubsequence(t, sub)
  }

  val lst1: MyList[Int] = MyList(1, 2, 3, 4, 5)
  val lst2: MyList[Int] = MyList(6, 7)
  val lst3: MyList[Int] = MyList(1, 2, 3, 4, 5, 6)
  val lst4: MyList[Int] = MyList(1, 2)
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