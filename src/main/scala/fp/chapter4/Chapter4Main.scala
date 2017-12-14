package fp.chapter4

import fp.chapter4.Chapter4Main.Option.{map2, map2For, sequence, traverse}

object Chapter4Main extends App {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }
  case class Some[+A](get: A) extends Option[A] {
    override def map[B](f: A => B): Option[B] = Some(f(get))
    override def flatMap[B](f: A => Option[B]): Option[B] = f(get)
    override def getOrElse[B >: A](default: => B): B = get
    override def orElse[B >: A](ob: => Option[B]): Option[B] = this
    override def filter(f: A => Boolean): Option[A] = if (f(get)) this else None
  }
  case object None extends Option[Nothing] {
    override def map[B](f: Nothing => B): Option[B] = None
    override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
    override def getOrElse[B >: Nothing](default: => B): B = default
    override def orElse[B >: Nothing](ob: =>Option[B]): Option[B] = ob
    override def filter(f: Nothing => Boolean): Option[Nothing] = None
  }

  object Option {
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f
    val abs0: Option[Double] => Option[Double] = lift(math.abs)

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch { case _: Exception => None }

    // ex 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(an => b.map(bn => f(an, bn)))

    def map2For[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      for {
        an <- a
        bn <- b
      } yield f(an, bn)

    // ex 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
      a match {
        case Nil => Some(Nil)
        case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
      }
    }

    // ex 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a match {
        case Nil => Some(Nil)
        case head :: tail => f(head).flatMap(b => traverse(tail)(f).map(b :: _))
      }
  }

  println("* ex 4.3")
  println(Option.map2   (Some(1), Some(2))(_ + _))
  println(Option.map2For(Some(1), Some(2))(_ + _))

  println("* ex 4.5")
  println(Option.traverse(List(1, 2, 3))(o => Some(o + 1)))


  // ex 4.6
  sealed trait Either[+A, +B] {
    def map[T](f: B => T): Either[A, T]
    def flatMap[U >: A, T](f: B => Either[U, T]): Either[U, T]
    def orElse[U >: A, T >: B](ob: => Either[U, T]): Either[U, T]
    def map2[U >: A, T, V](b: Either[U, T])(f: (B, T) => V): Either[U, V]
  }
  case class Right[+B](right: B) extends Either[Nothing, B] {
    override def map[T](f: B => T): Either[Nothing, T] = Right(f(right))
    override def flatMap[U >: Nothing, T](f: B => Either[U, T]): Either[U, T] = f(right)
    override def orElse[U >: Nothing, T >: B](ob: =>Either[U, T]): Either[U, T] = this
    override def map2[U >: Nothing, T, V](b: Either[U, T])(f: (B, T) => V): Either[U, V] = b.map(f(right, _))
  }
  case class Left [+A](left: A)  extends Either[A, Nothing] {
    override def map[T](f: Nothing => T): Either[A, T] = this
    override def flatMap[U >: A, T](f: Nothing => Either[U, T]): Either[U, T] = this
    override def orElse[U >: A, T >: Nothing](ob: =>Either[U, T]): Either[U, T] = ob
    override def map2[U >: A, T, V](b: Either[U, T])(f: (Nothing, T) => V): Either[U, V] = this
  }

  object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty) Left("mean of empty list!")
      else Right(xs.sum / xs.length)

    def saveDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e)}

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e)}

    // ex 4.7
    def sequence[A, B](a: List[Either[A, B]]): Either[A, List[B]] = {
      a match {
        case Nil => Right(Nil)
        case head :: tail => head.flatMap(h => sequence(tail).map(h :: _))
      }
    }

    def traverse[A, B, C](a: List[B])(f: B => Either[A, C]): Either[A, List[C]] =
      a match {
        case Nil => Right(Nil)
        case head :: tail => f(head).flatMap(b => traverse(tail)(f).map(b :: _))
      }
  }


}
