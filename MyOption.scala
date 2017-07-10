/**
  * Created by tao on 11/06/2017.
  */
import scala.collection.immutable.List

trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f) getOrElse MyNone

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(a) => a
  }
  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(MySome(_)) getOrElse ob

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(a) if f(a) => this
    case _ => MyNone
  }
}
case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyOption {
  def mean(xs: Seq[Double]): MyOption[Double] = {
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)
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
    //case Nil => MySome(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    //case h::t => h.flatMap(hh => hh)
  }

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    //case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_::_)
  }

  def Try[A](a: => A): MyOption[A] =
    try MySome(a)
    catch { case e: Exception => MyNone }
}

