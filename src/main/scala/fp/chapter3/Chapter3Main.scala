package fp.chapter3

import scala.annotation.tailrec

object Chapter3Main extends App {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }
    def product(doubles: List[Double]): Double = doubles match {
      case Nil => 1
      case Cons(0.0, _) => 0.0
      case Cons(head, tail) => head * product(tail)
    }
    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    // ex 3.1
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(n, Cons(2, Cons(4, _))) => n // 2の次は3なのでアンマッチ
      case Nil => 42 // 空リストじゃないのでアンマッチ
      case Cons(n, Cons(m, Cons(3, Cons(4, _)))) => n + m // マッチする、答え1 + 2で3
      case Cons(h, t) => h + sum(t) // マッチするけど↑が先なので☓
      case _ => 101 // マッチするけど↑が先なので☓
    }
    // ex 3.2
    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(_, r) => r // 再起したりしないのでlistの長さにかかわらず処理速度一定
    }
    // ex 3.3
    def setHead[A](x: A, list: List[A]): List[A] = {
      Cons(x, list) // こちらも処理速度一定
    }
    // ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = {
      if (n <= 0) l
      else l match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }
    // ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) if f(head) => dropWhile(tail, f)
        case _ => l
      }
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = {
      a1 match {
        case Nil => a2
        case Cons(head, tail) => Cons(head, append(tail, a2))
      }
    }

    // ex 3.6
    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => l
        case Cons(head, tail) => Cons(head, init(tail))
      }
    }

    // dropWhile型推論強化版
    def dropWhileStrong[A](l: List[A])(f: A => Boolean): List[A] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) if f(head) => dropWhile(tail, f)
        case _ => l
      }
    }

//    @tailrec
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      as match {
        case Nil => z
        case Cons(head, tail) => f(head, foldRight(tail, z)(f)) // 末尾再帰最適化できない
      }
    }
    def sum2(ns: List[Int]): Int = {
      foldRight(ns, 0)(_ + _)
    }
    def product2(ns: List[Double]): Double = {
      foldRight(ns, 1.0)(_ * _)
    }
//    foldRight(List(1, 2, 3), 0)(_ + _)
//    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _)
//    1 + foldRight(Cons(2, Cons(3, Nil)), 0)(_ + _)
//    1 + (2 + foldRight(Cons(3, Nil), 0)(_ + _))
//    1 + (2 + (3 + foldRight[Int, Int](Nil, 0)(_ + _)))
//    1 + (2 + (3 + 0))

    // ex 3.7
    // product2は0.0を検出した場合でもすぐに答えを返せるメソッドではない
    // productのようにパターンマッチで処理を分岐していないから
    // また５章で取り上げるってよ

    // ex 3.9
    def length[A](ns: List[A]): Int = {
      foldRight(ns, 0)((_, acc) => acc + 1)
    }

    // ex 3.10
    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(head, tail) => foldLeft(tail, f(z, head))(f)  // 末尾再帰最適化できる
      }
    }
    // ex 3.11
    def sum3(as: List[Int]): Int = {
      foldLeft(as, 0)(_ + _)
    }
    def product3(as: List[Double]): Double = {
      foldLeft(as, 1.0)(_ * _)
    }
    def length3[A](ns: List[A]): Int = {
      foldLeft(ns, 0)((acc, _) => acc + 1)
    }
    // ex 3.12
    def reverse[A](ns: List[A]): List[A] = {
      foldLeft(ns, Nil: List[A])((acc, o) => Cons(o, acc))
    }
    // ex 3.13 末尾再帰最適化可能なfoldRight
    def foldRightStrong[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
      foldLeft(reverse(as), z)((b, a) => f(a, b))
    }
    // ex 3.14
    def append2[A](a1: List[A], a2: List[A]): List[A] = {
      foldLeft(a1, a2)((b, a) => Cons(a, b))
    }
    // ex 3.15
//    def concat[A](ass: List[A]*): List[A] = {
//      foldLeft(List[List[A]].apply(ass), Nil:List[A])((acc, a) => append2(acc, a)) //.applyにしないと何故かコンパイルできない
//    }
    // ex 3.16
    def allPlus1(l: List[Int]): List[Int] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) => Cons(head + 1, allPlus1(tail))
      }
    }
    // ex 3.17
    def doublesToStrings(l: List[Double]): List[String] = {
      l match {
        case Nil => Nil
        case Cons(head, tail) => Cons(head.toString, doublesToStrings(tail))
      }
    }
    // ex 3.18 ついにmap登場！
    def map[A, B](as: List[A])(f: A => B): List[B] = {
      as match {
        case Nil => Nil
        case Cons(head, tail) => Cons(f(head), map(tail)(f))
      }
    }
    // ex 3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
      as match {
        case Nil => Nil
        case Cons(head, tail) => if (f(head)) Cons(head, filter(tail)(f)) else filter(tail)(f)
      }
    }
    // ex 3.20 みんなだいすきflatMap登場！
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      as match {
        case Nil => Nil
        case Cons(head, tail) => append2(f(head), flatMap(tail)(f))
      }
    }
    // ex 3.21
    def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(a => if (f(a)) List(a) else Nil)
    }
    // ex 3.22
    def zip(as: List[Int], bs: List[Int]): List[Int] = {
      (as, bs) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(ahead, atail), Cons(bhead, btail)) => Cons(ahead + bhead, zip(atail, btail))
      }
    }
    // ex 3.23
    def zipWith[A, B](a1s: List[A], a2s: List[A])(f: (A, A) => B): List[B] = {
      (a1s, a2s) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(ahead, atail), Cons(bhead, btail)) => Cons(f(ahead, bhead), zipWith(atail, btail)(f))
      }
    }
    // ex 3.24 不満
    def hasSubsequence[A](sup: List[A], sub:List[A]): Boolean = {
      if (length3(sup) < length3(sub)) false
      else {
        filter2(zipWith(sup, sub)(_ == _))(_ == false) match {
          case Nil => true
          case _ =>
            sup match {
              case Nil => false
              case Cons(_, tail) => hasSubsequence(tail, sub)
            }
        }
      }
    }
  }

  println(List.x)
  println(List.dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 4))
//  println(List.dropWhile(List(1, 2, 3, 4, 5), _ < 4)) // <- 型推論できない
  println(List.dropWhileStrong(List(1, 2, 3, 4, 5))(_ < 4)) // <- 型推論できる！(scalaの悲しい仕様)

  println("************ ex 3. 8 ************")
  // 意図不明。誰か教えて
  println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))

  println("************ ex 3.11 ************")
  println(List.length3(List(1, 2, 3, 4)))

  println("************ ex 3.12 ************")
  println(List.reverse(List(1, 2, 3, 4)))

  println("************ ex 3.23 ************")
  println(List.zipWith(List(1, 2, 3, 4), List(1))((_, _)))
  println(List.zipWith(List(1, 2, 3, 4), List(1, 2, 3, 4))((_, _)))
  println(List.zipWith(List(1, 2, 3, 4), List(1, 2, 3, 4, 5))((_, _)))

  println("************ ex 3.24 ************")
  println(List.hasSubsequence(List(1, 2, 3, 4), List(1))) // true
  println(List.hasSubsequence(List(1, 2, 3, 4), List(2))) // true
  println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3))) // true
  println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 4))) // false
  println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4))) // true
  println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2, 3, 4, 5))) // false

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    // ex 3.25
    def size[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right) // stack unsafe
      }
    }
    // ex 3.26
    def maximize(tree: Tree[Int]): Int = {
      tree match {
        case Leaf(value) => value
        case Branch(left, right) => maximize(left) max maximize(right)
      }
    }
    // ex 3.27
    def depth[A](tree: Tree[A]): Int = {
      tree match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + (depth(left) max depth(right))
      }
    }
    // ex 3.28
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      tree match {
        case Leaf(value) => Leaf(f(value))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }
    }
    // ex 3.29
    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
      tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
      }
    }
    def size2[A](tree: Tree[A]): Int =
      fold(tree)(_ => 1)(1 + _ + _)
    def maximize2(tree: Tree[Int]): Int =
      fold[Int, Int](tree)(identity)(_ max _)
    def depth2[A](tree: Tree[A]): Int =
      fold(tree)(_ => 1)((left, right) => 1 + (left max right))
    def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))
  }

  println("************ size ************")
  println(Tree.size (Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  println(Tree.size2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))

  println("************ maximize ************")
  println(Tree.maximize (Branch(Branch(Leaf(7), Branch(Leaf(2), Leaf(3))), Branch(Leaf(3), Leaf(4)))))
  println(Tree.maximize2(Branch(Branch(Leaf(7), Branch(Leaf(2), Leaf(3))), Branch(Leaf(3), Leaf(4)))))

  println("************ depth ************")
  println(Tree.depth (Branch(Branch(Leaf(7), Branch(Leaf(2), Leaf(3))), Branch(Leaf(3), Leaf(4)))))
  println(Tree.depth2(Branch(Branch(Leaf(7), Branch(Leaf(2), Leaf(3))), Branch(Leaf(3), Leaf(4)))))

  println("************ map ************")
  println(Tree.map (Branch(Branch(Leaf(7), Branch(Leaf(2), Leaf(3))), Branch(Leaf(3), Leaf(4))))(_ + 1))
  println(Tree.map2(Branch(Branch(Leaf(7), Branch(Leaf(2), Leaf(3))), Branch(Leaf(3), Leaf(4))))(_ + 1))
}
