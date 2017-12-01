package fp.chapter1

object Chapter1Main extends App {


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
}
