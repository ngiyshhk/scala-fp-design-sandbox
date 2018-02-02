package fp.chapter6

import fp.chapter6.Chapter6Main.{Rand, int, sequence}
import fp.chapter6.State.Coin

// ex 6.10
/**
  * ここから大幅ランクアップ
  * RNGの抽象化
  * もといStateモナドの作成
  * @param run
  * @tparam S
  * @tparam A
  */
case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State[S, B]({ s =>
    val (a, s2) = run(s)
    (f(a), s2)
  })
  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State[S, C]{ s =>
    val (a, s2) = run(s)
    val (b, s3) = sb.run(s2)
    (f(a, b), s3)
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B]{ s =>
    val (a, s2) = run(s)
    f(a).run(s2)
  }
}
object State extends App {
  def unit[S, A](a: A): State[S, A] = State[S, A](s => (a, s))
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = fs match {
    case a :: as => a.map2(sequence(as))(_ :: _)
    case Nil => unit[S, List[A]](Nil)
  }
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  trait RNG {
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  type Rand[+A] = State[RNG, A]
  val int: Rand[Int] = State(_.nextInt)
  def ints(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  val ns: Rand[List[Int]] =
    int.flatMap(x =>
      int.flatMap(y =>
        ints(x).map(xs =>
          xs.map(_ % y)
        )
      )
    )

  val ns2: Rand[List[Int]] =
    for {
      x <- int
      y <- int
      xs <- ints(x)
    } yield xs.map(_ % y)

  // ex 6.11
  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = inputs match {
    case Nil => unit((0, 0))
    case head :: tail => for {
      m1 <- get[Machine]
      _  <- set(inputMachine(head, m1))
      _  <- simulateMachine(tail)
      m  <- get[Machine]
    } yield (m.coins, m.candies)
  }
  def inputMachine(input: Input, machine: Machine) = input match {
    case Coin => machine.copy(locked = false, coins = machine.coins + 1)
    case Turn => machine.copy(locked = true, candies = machine.candies - 1)
  }

  println(simulateMachine(List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)).run(Machine(true, 5, 10)))
}

