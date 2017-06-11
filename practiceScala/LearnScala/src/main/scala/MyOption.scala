/**
  * Created by tao on 11/06/2017.
  */
import scala.collection.immutable.List

trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): MyOption[A] = this match {
    case None => None
    case Some(_) if f(_) => this
  }
}
case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]

object MyOption {
  def mean(xs: Seq[Double]): MyOption[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs) flatMap (m => mean(xs map (x => Math.pow(x - m, 2))))

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    a flatMap(aa => b map(bb => f(aa, bb)))

  def parseInsuranceRateQuotae(age: String, numberOfSpeedingTickets: String): MyOption[Double] = {
    val optAge: MyOption[Int] = Try(age.toInt)
    val optTickets: MyOption[Int] = Try(numberOfSpeedingTickets.toInt)
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
    age.toDouble + numberOfSpeedingTickets.toDouble
  }

  def sequence[A](l: List[MyOption[A]]): MyOption[List[A]] = l match {
    case Some(a: A) => Some(List(a))
    case h :: t => h.flatMap((hh => sequence(t).map(hh :: _)))
    //case h::t => h.flatMap(hh => hh)
  }

  def Try[A](a: => A): MyOption[A] =
    try Some(a)
    catch { case e: Exception => None }
}

