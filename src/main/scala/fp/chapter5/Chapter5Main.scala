package fp.chapter5

import scala.annotation.tailrec

object Chapter5Main extends App {
  trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }
    // ex 5.1
    def toList: List[A] = {
      @tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, t) => go(t(), h() :: acc)
      }
      go(this, List()).reverse
    }
    // ex 5.2
    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (n <= 0) Empty else Cons(h, () => t().take(n - 1))
    }
    def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(_, t) => if (n <= 1) t() else t().drop(n - 1)
    }
    // ex 5.3
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, t) => if (p(h())) Cons(h, () => t().takeWhile(p)) else Empty
    }
    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
    def exists2(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b) //非正格だとコードの再利用性が高まっている！
    // ex 5.4
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)
    // ex 5.5
    def takeWhile2(p: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((a, b) => if (p(a)) Stream.cons(a, b) else Empty)
    // ex 5.6
    def headOption2: Option[A] =
      foldRight[Option[A]](None)((a, _) => Some(a))
    // ex 5.7
    def map[B](f: A => B): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => Stream.cons(f(a), b))
    def filter(f: A => Boolean): Stream[A] =
      foldRight[Stream[A]](Empty)((a, b) => if (f(a)) Stream.cons(a, b) else b)
    def append[B >: A](d: => Stream[B]): Stream[B] =
      foldRight[Stream[B]](d)((a, b) => Stream.cons[B](a, b))
    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight[Stream[B]](Empty)((a, b) => f(a).append(b))
    def find(p: A => Boolean): Option[A] =
      filter(p).headOption

  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }
    // ex 5.8
    def constant[A](a: A): Stream[A] = {
      lazy val st: Stream[A] = cons(a, st)
      st
    }
    // ex 5.9
    def from(n: Int): Stream[Int] =
      Stream.cons(n, from(n + 1))
    // ex 5.10
    val fibs: Stream[Int] = {
      def go(pre: Int, cur: Int): Stream[Int] =
        Stream.cons(pre, go(cur, pre + cur))
      go(0, 1)
    }
    // ex 5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, s)) => Stream.cons(a, unfold(s)(f))
        case None => Stream.empty[A]
      }
    }
    // ex 5.12
    val fibs2: Stream[Int] =
      unfold((0, 1))(s => Some((s._1, (s._2, s._1 + s._2))))
    def from2(n: Int): Stream[Int] =
      unfold(n)(s => Some((s, s + 1)))
    def constant2(n: Int): Stream[Int] =
      unfold(n)(s => Some((s, s)))
    val ones2: Stream[Int] =
      unfold(1)(_ => Some((1, 1)))
  }

  println(Stream(1, 2, 3).toList)
  println(Stream(1, 2, 3, 4, 5, 6).take(3).toList)
  println(Stream(1, 2, 3, 4, 5, 6).drop(4).toList)
  println(Stream(1, 2, 3, 4, 5, 6).takeWhile(_ <= 4).toList)
  println(Stream(1, 2, 3, 4, 5, 6).exists(_ > 5))
  println(Stream(1, 2, 3, 4, 5, 6).foldRight(0)((i, d) => d + i))
  println(Stream(1, 2, 3, 4, 5, 6).exists (i => {println(i); i > 3}))
  println(Stream(1, 2, 3, 4, 5, 6).exists2(i => {println(i); i > 3}))
  println(Stream(1, 2, 3, 4, 5, 6).forAll (i => {println(i); i > 3}))
  println(Stream(1, 2, 3, 4, 5, 6).forAll (i => {println(i); i > 0}))
  println(Stream(1, 2, 3, 4, 5, 6).takeWhile2(i => {println(i); i <= 4}).toList)
  println(Stream(1, 2).headOption)
  println(Stream(1, 2).headOption2)
  println(Empty.headOption)
  println(Empty.headOption2)
  println(Stream(1, 2, 3, 4, 5, 6).map(i => {println(i); i > 3}).toList)
  println(Stream(1, 2, 3, 4, 5, 6).filter(i => {println(i); i > 3}).toList)
  println(Stream(1, 2, 3).append({ println("aaa"); Stream(4 + 1) }).toList)
  println(Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList)

  // トレース
  Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  Stream.cons(11, Stream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
  Stream(2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  Stream.cons(12, Stream(3, 4).map(_ + 10)).filter(_ % 2 == 0).toList
  12 :: Stream(3, 4).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: Stream.cons(13, Stream(4).map(_ + 10)).filter(_ % 2 == 0).toList
  12 :: Stream(4).map(_ + 10).filter(_ % 2 == 0).toList
  12 :: Stream.cons(14, Stream[Int]().map(_ + 10)).filter(_ % 2 == 0).toList
  12 :: 14 :: Stream[Int]().map(_ + 10).filter(_ % 2 == 0).toList
  12 :: 14 :: List()

  val ones: Stream[Int] = Stream.cons(1, ones)
  println(ones.take(10).toList)
  println(ones.exists(_ % 2 != 0))
  println(ones.map(_ + 1).exists(_ % 2 == 0))
  println(ones.takeWhile(_ == 1).take(10).toList)
  println(ones.forAll(_ != 1))

  println(Stream.constant("abc").take(10).toList)
  println(Stream.from(5).take(10).toList)

  println(Stream.fibs.take(10).toList)

  println(Stream.unfold(0)(s => Some(s + 1, s + 1)).take(10).toList)
  println(Stream.unfold((0, 1))(s => Some(s._1, (s._2, s._1 + s._2))).take(10).toList)
}
