
sealed trait MyStream[+A] {
  def headOption: Option[A] = this match {
    case MyEmpty => None
    case MyCons(h, t) => Some(h())
  }
  def toList: List[A] = {
    def convertToList(stream: MyStream[A], list: List[A]): List[A] = stream match {
      case MyCons(h, t) => convertToList(t(), h()::list)
      case _ => list
    }
    convertToList(this, List()).reverse
  }

  def take(n: Int): MyStream[A] = this match {
    case MyCons(h, t) if n > 1 => MyStream.mycons(h(), t().take(n-1))
    case MyCons(h, _) if n == 1 => MyStream.mycons(h(), MyStream.myempty)
    case _ => MyStream.myempty
  }

  def drop(n: Int): MyStream[A] = this match {
    case MyCons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): MyStream[A] = this match {
    case MyCons(h, t)  => if (p(h())) MyStream.mycons(h(), t().takeWhile(p)) else t().takeWhile(p)
    case _ => MyStream.myempty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case MyCons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhileWithFoldRight(p: A => Boolean): MyStream[A] =
    foldRight(MyStream.myempty[A])((a, b) => if (p(a)) MyStream.mycons(a, b) else MyStream.myempty)

  def headOptionWithFoldRight(): Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def constant[A](a: A): MyStream[A] = {
    lazy val tail: MyStream[A] = MyCons(() => a, () => tail)
    tail
  }

  def from(n: Int): MyStream[Int] = {
    MyStream.mycons(n, from(n+1))
  }

  def fibs = {
    def addition(n1: Int, n2: Int): MyStream[Int] = {
      MyStream.mycons(n1, addition(n2, n1+n2))
    }
    addition(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): MyStream[A] = f(z) match {
    case Some((h,s)) => MyStream.mycons(h, unfold(s)(f))
    case None => MyEmpty
  }

  def fibsWithUnFold = unfold((0,1))({case (f0, f1) => Some((f0, (f1, f0+f1)))})

  def fromWithUnFold(n: Int): MyStream[Int] = unfold(n)(n => Some(n, n+1))

  def constantWithUnFold[A](a: A): MyStream[A] = unfold(a)(_ => Some(a, a))

  def map[B](f: A => B): MyStream[B] = unfold(this)({
    case MyCons(h, t) => Some((f(h()), t()))
    case _ => None
  })

  def zipAll[B](s2: MyStream[B]): MyStream[(Option[A], Option[B])] = unfold(this, s2)({
    case (MyCons(h1, t1), MyCons(h2, t2)) => Some((Some(h1()), Some(h2())) -> (t1(), t2()))
    case (MyCons(h1, t1), MyEmpty) => Some((Some(h1()), None) -> (t1(), MyEmpty))
    case (MyEmpty, MyCons(h2, t2)) => Some((None, Some(h2())) -> (MyEmpty, t2()))
    case (_, _) => None
  })

  def startsWith[A](s: MyStream[A]): Boolean = ???
}

case object MyEmpty extends MyStream[Nothing]
case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]

object MyStream {
  def mycons[A](hd: => A, ts: => MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = ts
    MyCons(() => head, () => tail)
  }

  def myempty[A]: MyStream[A] = MyEmpty

  def apply[A](as: A*): MyStream[A] =
    if (as.isEmpty) myempty else mycons(as.head, apply(as.tail: _*))
}

