/**
  * Created by motoyasu.saburi on 2017/07/23.
  */


sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
  def filter2(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
  def absO: Option[Double] => Option[Double] = lift(math.abs)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }
}


case class Employee(name: String, department: String) extends Option[String]
def lookupByName(name: String): Option[Employee] = ???
val jobDepartment: Option[String] = lookupByName("Joe").map(_.department)
