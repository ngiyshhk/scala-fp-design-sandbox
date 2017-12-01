package fp.chapter1

object Chapter1Main extends App {
  {
    println("----------------------------------")
    println("---            V1              ---")
    println("----------------------------------")
    val version = V1()
    import version._
    val cc = CreditCard(100)
    println(s"1: $cc")
    val cup1 = buyCoffee(cc)
    println(s"2: $cc, $cup1")
    val cup2 = buyCoffee(cc)
    println(s"3: $cc, $cup2")
  }

  {
    println("----------------------------------")
    println("---            V2              ---")
    println("----------------------------------")
    val version = V2()
    import version._
    val cc = CreditCard(100)
    val p = Payments()
    println(s"1: $cc")
    val cup1 = buyCoffee(cc, p)
    println(s"2: $cc, $p, $cup1")
    val cup2 = buyCoffee(cc, p)
    println(s"3: $cc, $p, $cup2")
  }

  {
    println("----------------------------------")
    println("---            V3              ---")
    println("----------------------------------")
    val version = V3()
    import version._
    val cc = CreditCard(100)
    println(s"1: $cc")
    val (cup1, charge1) = buyCoffee(cc)
    println(s"2: $cc, $cup1, $charge1")
    val (cup2, charge2) = buyCoffee(cc)
    println(s"3: $cc, $cup2, $charge2")

    {
      val charges = charge1.combine(charge2)
      println(s"combined: $charges")
    }
    {
      val (coffees, charge) = buyCoffees(cc, 100)
      println(s"buy 100 cups of coffee: $coffees, $charge")
    }
    {
      val cc2 = CreditCard(200)
      val cc3 = CreditCard(300)

      val charges = (1 to 10).map(_ => buyCoffee(cc)._2).toList ++
        (1 to 20).map(_ => buyCoffee(cc2)._2).toList ++
        (1 to 30).map(_ => buyCoffee(cc3)._2).toList
      val coalesced: List[Charge] = coalesce(charges)
      println(s"coalesce: $coalesced")
    }
  }

  {
    println("----------------------------------")
    println("---          参照透過           ---")
    println("----------------------------------")
    val version = SanshoToka()
    import version._
    execute()
  }
}

case class V1() {
  case class CreditCard(var charge: Double) {
    def charge(price: Double): Unit = {
      this.charge = this.charge + price
    }
  }
  case class Coffee(price: Double = 100)

  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = Coffee()
    cc.charge(cup.price) // 副作用。クレジットカードにチャージしている
    cup
  }
}

case class V2() {
  case class CreditCard(var charge: Double) {
    def charge(price: Double): Unit = {
      this.charge = this.charge + price
    }
  }
  case class Coffee(price: Double = 100)
  case class Payments() {
    def charge(cc: CreditCard, price: Double): Unit = {
      cc.charge(price)
    }
  }

  def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
    val cup = Coffee()
    p.charge(cc, cup.price) // まだ副作用。
    cup
  }
}

case class V3() {
  case class CreditCard(var charge: Double) {
    def charge(price: Double): Unit = {
      this.charge = this.charge + price
    }
  }
  case class Coffee(price: Double = 100)
  case class Charge(cc: CreditCard, amount: Double) {
    def combine(other: Charge): Charge = {
      if (cc == other.cc) Charge(cc, amount + other.amount)
      else throw new RuntimeException("other cc.")
    }
  }

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = Coffee()
    (cup, Charge(cc, cup.price)) // 副作用なし！
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = { // 複数のコーヒーを買う関数がこんなにかんたんに！
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce(_ combine _))
  }

  def coalesce(charges: List[Charge]): List[Charge] = { // カードごとのチャージを作る関数がこんなにかんたんに！
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
  }
}

case class SanshoToka() {
  def execute(): Unit = {
    {
      {
        println("----------------------------------")
        val x = "Hello, World"
        val r1 = x.reverse
        val r2 = x.reverse
        println(x)
        println(r1)
        println(r2)
      }
      // 同じ = xは参照透過
      {
        println("----------------------------------")
        val x = "Hello, World"
        val r1 = "Hello, World".reverse
        val r2 = "Hello, World".reverse
        println(x)
        println(r1)
        println(r2)
      }
    }

    {
      {
        println("----------------------------------")
        val x = new StringBuilder("Hello")
        val y = x.append(", World")
        val r1 = y.toString()
        val r2 = y.toString()
        println(x)
        println(y)
        println(r1)
        println(r2)
      }
      // 違う = yは参照透過じゃない
      {
        println("----------------------------------")
        val x = new StringBuilder("Hello")
        val r1 = x.append(", World").toString()
        val r2 = x.append(", World").toString()
        println(x)
        println(r1)
        println(r2)
      }
    }
  }
}