import scala.collection.mutable.ListBuffer

/**
  * Created by motoyasu.saburi on 2017/07/02.
  */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], r: A): List[A] = l match {
    case Nil => Nil
    case Cons(_, t) => Cons(r, this.tail(t))
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    l match {
      case Nil => Nil
      case Cons(h, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case l => l
    }
  }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("init of empty list")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }

  def init2[A](l: List[A]): List[A] = {
    val buf = new ListBuffer[A]
    def go(cur: List[A]): List[A] = cur match {
      case Nil => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int = {
//    foldRight(as, 0)((x, y) => length(x, y + 1))
    foldRight(as, 0)((_, acc) => acc + 1)
  }

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(as, h))(f)

    }
  }

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc,h) => Cons(h,acc))
  def append2[A](l: List[A]): List[A] = foldLeft(l, List[A]())()

  def main(args: List[String]): Unit = {
    val list = List(0.0)

    println("a")
  }

  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil:List[String])((h,t) => Cons(h.toString, t))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
  def filter[A, B](l: List[A])(f: A => A): List[A] = {
    l match {
      case Cons(h,t) if(f(l)) => Cons(h, t)
      case Nil => Nil
      case Cons(h, t) => Cons(Nil, t)
    }
    foldLeft(l, Nil:List[A])
  }

  def addOne(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h,t) => Cons(h+1,t))


}
