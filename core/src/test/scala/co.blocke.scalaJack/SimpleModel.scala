package co.blocke.scalajack
package test

trait Blah
case class Foo(
	name:String, 
	age:Int) extends Blah
case class Stuff(
	item:String,
	other:Blah
	)

// Parameterized classes/traits
case class Case_1( name:String, other:List[WithType[Int]] )
case class Case_2[T]( name:String, other:List[WithType[T]] )
case class Case_3[T]( name:String, other:List[Two[T,Boolean]] )
case class Case_5[U,T]( name:String, other:List[Two[T,U]] )
case class WithType[T](me:T)
case class Two[T,U](a:T, b:U)
trait Excite[T,U] {
	val a:U
	val b:T
}
trait Excite2[T,U] {
	val a:T
	val b:U
}
trait Sleep[Y] {
	val x:Y
}
case class Ex1(a:String, b:Int) extends Excite[Int,String]
case class Slp[Z](x:Z) extends Sleep[Z]
case class Ex2[Z](a:Sleep[Z], b:Int) extends Excite2[Sleep[Z],Int]
// case class Ex2[Z](a:Slp[Z], b:Int) extends Excite[Int,Sleep[Z]]

case class All(
	a:	Int,
	b:	java.lang.Integer,
	c:	Boolean,
	d:	java.lang.String,
	e:	String,
	f:	Float,
	g:	Double,
	h:	Long,
	i:	Char,
	j:	String, // set to null
	k:	Byte,
	l:	Short,
	m:  java.util.UUID
	)
case class AllColl(
	a: List[Int],
	b: List[Foo],
	c: Option[Int],
	d: Option[String],
	e: List[Option[Int]],
	f: Map[String,Int],
	g: Map[Foo,Option[WithType[Int]]] // test sloppy
 	)

object Colors extends Enumeration {
  val Red, Amber, Green = Value
}
import Formats._  // try alternate Ennumeration form
case class EnumExer( a:Colors.Value, b:Format )

// Value classes
class Wrapper(val underlying: String) extends AnyVal
class Wrapper2[T](val underlying: T) extends AnyVal
case class Wrapped( hey:Wrapper, you:Int )
case class Wrapped2[T]( hey:Wrapper2[T], you:Int )

case class Address(street:String, zip:Int)
case class Pristine( name:String, age:Int, stuff:Option[Boolean], addr:Address )
