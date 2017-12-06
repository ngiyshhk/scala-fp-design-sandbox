package fp.chapter2

import scala.annotation.tailrec

object Chapter2Main extends App {
  {
    val version = V1()
    import version._
    println(formatAbs(-42))
    println(factorial(5))
    println(factorial2(5))
    println((0 to 10).map(fib))
    println(formatFactorial(5))
    println(formatResult("absolute value by formatResult", -42, abs))
    println(formatResult("factorial by formatResult", 5, factorial))
    println(findFirst(Array("a", "b", "c", "d"), "b"))
  }

  {
    val version = V2()
    import version._
    println(findFirst(Array("a", "b", "c", "d"), (s: String) => s == "b"))
    println(isSorted[Int](Array(1, 2, 3, 4, 5), _ <= _))
    println(isSorted[Int](Array(1, 2, 1, 4, 5), _ <= _))
    println(curry[Int, Int, Int](_ + _)(2)(3))
    println(uncurry(curry[Int, Int, Int](_ + _))(2, 3))
    println(compose[Int, Int, Int](_ * 5, _ + 2)(2))
    println((((i: Int) => i + 2) andThen(_ * 5))(2))
  }

  case class V1() {
    def abs(n: Int): Int = {
      if (n < 0) -n
      else n
    }
    def formatAbs(x: Int): String = {
      val msg = "The absolute value of %d is %d"
      msg.format(x, abs(x))
    }
    def formatFactorial(x: Int): String = {
      val msg = "The factorial of %d is %d"
      msg.format(x, factorial(x))
    }
    def formatResult(name: String, x: Int, f: Int => Int): String = {
      val msg = s"The $name of %d is %d"
      msg.format(x, f(x))
    }

    def factorial(n: Int): Int = {
      @tailrec
      def go(x: Int, acc: Int): Int = {
        if (x <= 0) acc
        else go(x - 1, x * acc)
      }
      go(n, 1)
    }

    // factorialは末尾再帰最適化で下記のようなコードにコンパイルされる
    def factorial2(n: Int): Int = {
      var i = n
      var acc = 1
      while(i > 0) {
        acc = acc * i
        i = i - 1
      }
      acc
    }

    // ex 2.1
    def fib(n: Int): Int = {
      @tailrec
      def go(x: Int, next: Int, current: Int): Int = {
        if (x == 0) current
        else go(x - 1, next + current, next)
      }
      go(n, 1, 0)
    }

    def findFirst(ss: Array[String], key: String): Int = {
      @tailrec
      def loop(n: Int): Int = {
        if (n >= ss.length) -1
        else if (ss(n) == key) n
        else loop(n + 1)
      }
      loop(0)
    }
  }

  case class V2() {
    // 多層関数化
    def findFirst[T](ss: Array[T], f: T => Boolean): Int = {
      @tailrec
      def loop(n: Int): Int = {
        if (n >= ss.length) -1
        else if (f(ss(n))) n
        else loop(n + 1)
      }
      loop(0)
    }

    // ex 2.2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @tailrec
      def loop(n: Int): Boolean = {
        if (n + 1 >= as.length) true
        else if (!ordered(as(n), as(n + 1))) false
        else loop(n + 1)
      }
      loop(0)
    }

    // ex 2.3
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
      (a: A) => (b: B) => f(a, b)
    }

    // ex 2.4
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
      (a: A, b: B) => f(a)(b)
    }

    // ex 2.5
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
      (a: A) => f(g(a))
    }
  }
}
