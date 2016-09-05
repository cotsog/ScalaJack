package co.blocke.scalajack
package test

import org.scalatest.{ FunSpec, GivenWhenThen, BeforeAndAfterAll }
import org.scalatest.Matchers._

// Case 1 -------- > Simple parameterized case class
case class Boom[T](a: T)

// Case 2 -------- > Class having a parameterized trait
trait Thing[T, U] {
  val t: T
  val u: U
}
case class OneThing(t: String, u: Int) extends Thing[String, Int]
case class Wow(a: String, b: Thing[String, Int])

// Case 3 -------- > Parameterized class having a parameterized trait
trait Baba[T] { val a: T }
case class Mibu[X](a: X, q: Boolean) extends Baba[X]
case class Wawa[A](a: Baba[A], b: String)

// Case 4 -------- > Parameterized case class implementing a parameterized trait
trait Thing2[T, U] {
  val t: T
  val u: U
}
case class TwoThing[P](x: P, t: String, u: P) extends Thing2[String, P]
case class Wow2[A](a: String, b: Thing2[String, A])

// Case X
case class Two(
  foo: String,
  bar: Boolean
)
trait Tart[T] {
  val yum: T
}
case class Toast[D](g: Int, val yum: D) extends Tart[D]
case class Breakfast[K](y: Boolean, bread: Tart[K])

class Foo extends FunSpec with GivenWhenThen with BeforeAndAfterAll {
  val sj = ScalaJack()
  val old = ScalaJack(json.JsonFlavor())
  describe("-- Cases --") {
    /*
    it("Case 1") {
      val b = Boom(true)
      val js = sj.render(b)
      println(js)
      val obj = sj.read[Boom[Boolean]](js)
      println(obj)
    }
    it("Case 2") {
      val m = OneThing("xix", 5)
      val js = sj.render(Wow("ok", m))
      println(js)
      val obj = sj.read[Wow](js)
      println(obj)
    }
    // Broken in old version!!!  Yay!
    // it("Old") {
    //   val m = TwoThing(1, "xix", 5)
    //   val js = old.render(Wow2("ok", m))
    //   println(js)
    //   val obj = old.read[Wow2[Int]](js)
    //   println(obj)
    // }
    it("Case 3") {
      scala.util.Try {
        val mb = Mibu(5, true)
        val ww = Wawa(mb, "yep")
        val js = sj.render(ww)
        println(js)
        val obj = sj.read[Wawa[Int]](js)
        println(obj)
      }
    }
    it("Case 4") {
      scala.util.Try {
        val m = TwoThing(99, "xix", 5)
        val js = sj.render(Wow2("ok", m))
        println(js)
        val obj = sj.read[Wow2[Int]](js)
        println(obj)
      }
    }
    */
    it("Case class having an embedded parameterized trait") {
      val w = Breakfast(true, Toast(7, "Burnt"))
      val js = ScalaJack().render(w)
      println(js)
      js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":"Burnt"}}""")
      ScalaJack().read[Breakfast[String]](js) should equal(w)
      println("-----------------------")
    }
    it("Case class having an embedded parameterized trait, with the trait's parameter another case class") {
      val w = Breakfast(true, Toast(7, Two("two", true)))
      val js = sj.render(w)
      println(js)
      js should equal("""{"y":true,"bread":{"_hint":"co.blocke.scalajack.test.Toast","g":7,"yum":{"foo":"two","bar":true}}}""")
      sj.read[Breakfast[Two]](js) should equal(w)
    }
  }
}