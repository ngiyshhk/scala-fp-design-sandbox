package fp.variant

import scala.runtime.java8.JFunction2$mcIJI$sp

object VariantMain extends App {
  class Jiji
  class Papa extends Jiji
  class Kodo extends Papa


  case class CoVariant[+T](t: T)

  val covariant1: CoVariant[Papa] = CoVariant(new Kodo) // ok
  println(covariant1.t)
//val coVariant2: CoVariant[Papa] = CoVariant(new Jiji) // ng

  case class InVariant[-T]() {
    def get[T1 <: T](x: T1): T1 = {
      x
    }
  }

  val invariant = InVariant[Papa]()
  println(invariant.get(new Kodo))
  println(invariant.get(new Papa))
//  println(invariant.get(new Jiji)) ng
}
