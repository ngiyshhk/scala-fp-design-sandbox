package fp.chapter6

object Chapter6Main extends App {
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
  {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    val (n2, rng3) = rng2.nextInt
    Seq[Any](rng, rng2, rng3, n1, n2).foreach(println)
  }

  // ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (tmp, rng2) = rng.nextInt
    (if (tmp < 0) (tmp + 1) * -1 else tmp, rng2)
  }

  {
    val rng = SimpleRNG(42)
    val (n1, rng2) = nonNegativeInt(rng)
    val (n2, rng3) = nonNegativeInt(rng2)
    Seq[Any](rng, rng2, rng3, n1, n2).foreach(println)
  }

  // ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (tmp, rng2) = nonNegativeInt(rng)
    (tmp.toDouble / Integer.MAX_VALUE.toDouble, rng2)
  }

  {
    val rng = SimpleRNG(42)
    val (n1, rng2) = double(rng)
    val (n2, rng3) = double(rng2)
    Seq[Any](rng, rng2, rng3, n1, n2).foreach(println)
  }

  // ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (d2, rng3) = double(rng2)
    ((i1, d2), rng3)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d1, rng2) = double(rng)
    val (i2, rng3) = rng2.nextInt
    ((d1, i2), rng3)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 => (Nil, rng)
      case _ =>
        val (i, rng2) = rng.nextInt
        val (is, rngz) = ints(count - 1)(rng2)
        (i :: is, rngz)
    }
  }

  {
    val rng = SimpleRNG(42)
    println(ints(2)(rng))
    println(ints(10)(rng))
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // ex 6.5
  def double2: Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / Integer.MAX_VALUE.toDouble)

    {
      val rng = SimpleRNG(42)
      val (n1, rng2) = double2(rng)
      val (n2, rng3) = double2(rng2)
      Seq[Any](rng, rng2, rng3, n1, n2).foreach(println)
    }

  // ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rngc =>
      val (a, rnga) = ra(rngc)
      val (b, rngb) = rb(rnga)
      (f(a, b), rngb)
  }

  {
    println(map2(nonNegativeEven, double2)((_, _))(SimpleRNG(42)))
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  {
    println(both(nonNegativeEven, double2)(SimpleRNG(42)))
  }

  // ex 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case a :: as => map2(a, sequence(as))(_ :: _)
    case Nil => unit[List[A]](Nil)
  }
  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  {
    val rng = SimpleRNG(42)
    println(ints2(2)(rng))
    println(ints2(10)(rng))
  }
}
