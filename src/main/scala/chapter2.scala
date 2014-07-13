object Fibonacci {

  def fib(n: Int): Int = {
    require(n > 0)
    @annotation.tailrec
    def go(i: Int, prev: Int, acc: Int): Int = if (i == n) acc else go(i + 1, acc, acc + prev)
    if (n == 1) 0 else go(2, 0, 1)
  }

  def main(args: Array[String]) {
    for (i <- 1 to 6)
      println(fib(i))
  }

}

object CheckSorted {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(i: Int): Boolean = {
      if (i + 1 == as.length) true
      else if (gt(as(i), as(i + 1))) false
      else loop(i + 1)
    }
    if (as.length <= 1) true
    else loop(0)
  }

  def main(args: Array[String]) {
    println(isSorted[Int](Array(), _ > _))
    println(isSorted[Int](Array(2, 1), _ > _))
    println(isSorted[Int](Array(3, 5, 6), _ > _))
    println(isSorted[Int](Array(4, 4, 4), _ > _))
    println(isSorted[Int](Array(4, 4, 6), _ > _))
  }

}

object Currying {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def main(args: Array[String]) {
    println(curry[Int, Int, Int](_ + _)(1)(2))
    println(uncurry(curry[Int, Int, Int](_ + _))(1, 2))
  }

}

object Compose {

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def main(args: Array[String]) {
    println(compose[Int, String, String](_.reverse, x => (x  + 1).toString)(23))
  }

}
