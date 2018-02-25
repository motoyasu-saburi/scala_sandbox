/**
  * Created by motoyasu.saburi on 2017/07/02.
  */
object test {
  def curry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a: A)(b: B)
  }
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
  def main(args: Array[String]): Unit = {
    println("hoge")
  }
}
